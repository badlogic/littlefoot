// prettier-ignore
import {AstNode, DoNode, ForEachNode, ForNode, FunctionLiteralNode, FunctionNode, IsOperatorNode, LoopVariable, NameAndTypeNode, RecordLiteralNode, ReturnNode, TypeNode, TypeSpecifierNode, VariableAccessNode, VariableNode, WhileNode, traverseAst,} from "./ast";
import { CompilerContext, Module } from "./compiler";
import { LittleFootError } from "./error";
import { SourceLocation } from "./source";
// prettier-ignore
import { BooleanType, FunctionType, ListType, MapType, NameAndType, NamedFunction, NamedType, NothingType, NumberType, RecordType, ResolvingTypeMarker, StringType, Type, UnionType, UnknownType, isAssignableTo as typeIsAssignableTo, isEqual, traverseType } from "./types";

function assertNever(x: never) {
  throw new Error("Unexpected object: " + x);
}

export type Symbol = VariableNode | NameAndTypeNode | LoopVariable;

export class SymbolScopes {
  scopes = new Array<Map<String, Symbol>>();

  constructor() {
    this.push();
  }

  push() {
    this.scopes.push(new Map<String, Symbol>());
  }

  pop() {
    this.scopes.pop();
  }

  get(name: string): Symbol | undefined {
    let scopes = this.scopes;
    for (var i = scopes.length - 1; i >= 0; i--) {
      let scope = scopes[i];
      let symbol = scope.get(name);
      if (symbol) {
        return symbol;
      }
    }
    return undefined;
  }

  add(node: Symbol, allowShadow = true) {
    let scopes = this.scopes;
    for (var i = scopes.length - 1; i >= 0; i--) {
      let scope = scopes[i];
      let other = scope.get(node.name.value);
      if (other) {
        throw new LittleFootError(node.name.location, `Variable ${node.name.value} already defined in line ${other.name.location.lines[0].index}.`);
      }
      if (allowShadow) break;
    }
    scopes[scopes.length - 1].set(node.name.value, node);
  }
}

export class TypeCheckerContext {
  constructor(
    public readonly module: Module,
    public readonly compilerContext: CompilerContext,
    public readonly scopes = new SymbolScopes(),
    private currentLoop: (ForNode | ForEachNode | WhileNode | DoNode)[] = []
  ) {}

  pushLoop(loop: ForNode | ForEachNode | WhileNode | DoNode) {
    this.currentLoop.push(loop);
  }

  popLoop() {
    this.currentLoop.length = this.currentLoop.length - 1;
  }

  getCurentLoop(): ForNode | ForEachNode | WhileNode | DoNode | undefined {
    if (this.currentLoop.length == 0) return undefined;
    else return this.currentLoop[this.currentLoop.length - 1];
  }
}

export function checkTypes(context: TypeCheckerContext) {
  const { ast, types, functions } = context.module;
  const errors = context.compilerContext.errors;

  // Gather named type nodes. These are named types that other
  // types may refer to. Set their type to UnknownType.
  const namedTypes: TypeNode[] = ast.filter((node) => node.kind == "type declaration") as TypeNode[];
  for (const type of namedTypes) {
    try {
      type.type = new NamedType(type.name.value, UnknownType, type, type.location);
      types.add(type.type as NamedType);
    } catch (e) {
      if (e instanceof LittleFootError) errors.push(e);
      else
        errors.push(
          new LittleFootError(new SourceLocation(type.location.source, 0, 1), "Internal error: " + (e as any).message + "\n" + (e as any).stack)
        );
      return;
    }
  }

  // For each named type, replace their UnknownType with the real type by
  // recursive type resolution in checkNodeTypes() cases "type reference" and
  // "type declaration".
  for (const type of namedTypes) {
    try {
      checkNodeTypes(type, context);
    } catch (e) {
      if (e instanceof LittleFootError) errors.push(e);
      else
        errors.push(
          new LittleFootError(new SourceLocation(type.location.source, 0, 1), "Internal error: " + (e as any).message + "\n" + (e as any).stack)
        );
      return;
    }
  }
  if (errors.length > 0) return;

  // Generate constructor functions for every named type that is a record.
  for (const type of namedTypes) {
    if (type.type.kind == "named type" && type.type.type.kind == "record") {
      const funcType = new FunctionType(type.type.type.fields, type.type);
      const func = new NamedFunction(
        type.name.value,
        funcType,
        new FunctionLiteralNode(type.name, [], null, [], type.name.location),
        true,
        true,
        type.name.location
      );
      functions.add(func);
    }
  }

  // Gather named functions and assign their parameter types. The return type is
  // assigned lazily once the function is encountered during AST traversal below,
  // through a second call to `checkFunctionNode()`.
  const namedFunctions = ast.filter((node) => node.kind == "function declaration") as FunctionNode[];
  for (const func of namedFunctions) {
    const functionType = checkFunctionNode(func, context, false);
    const namedFunction = new NamedFunction(func.name.value, functionType, func, func.exported, func.external, func.location);
    functions.add(namedFunction);
  }

  // All named types are defined, assign and check the types of all AST nodes. This will also add all named functions
  // to module.functions, see the "function declaration" case in checkNodeTypes().
  for (const node of ast) {
    // TODO recover in case a statement or expression throws an error and type check the remainder of the AST if possible
    // This should work for errors within functions. For top-level statements, stop if a var declaration fails.
    try {
      checkNodeTypes(node, context);
    } catch (e) {
      if (e instanceof LittleFootError) errors.push(e);
      else errors.push(new LittleFootError(node.location, "Internal error: " + (e as any).message + "\n" + (e as any).stack));
      return;
    }
  }

  // Finally check that we have no unknown and named types in the AST.
  for (const node of ast) {
    // Ignore type and function declaration nodes
    if (node.kind == "type declaration") continue;
    try {
      traverseAst(node, (node) => {
        traverseType(node.type, (type) => {
          if (type == UnknownType) {
            throw new LittleFootError(node.location, "Internal error: AST node has unknown type.");
          }
          return true;
        });
        if (node.kind != "type reference" && node.kind != "name and type") {
          if (node.type.kind == "named function" || node.type.kind == "named type") {
            // throw new LittleFootError(node.location, "Internal error: AST node has named type.");
          }
        }
        return true;
      });
    } catch (e) {
      if (e instanceof LittleFootError) errors.push(e);
      else errors.push(new LittleFootError(node.location, "Internal error: " + (e as any).message + "\n" + (e as any).stack));
      return;
    }
  }
}

export function checkNodeTypes(node: AstNode, context: TypeCheckerContext) {
  const types = context.module.types;
  const functions = context.module.functions;
  const scopes = context.scopes;

  switch (node.kind) {
    case "nothing": {
      node.type = NothingType;
      break;
    }
    case "boolean": {
      node.type = BooleanType;
      break;
    }
    case "number": {
      node.type = NumberType;
      break;
    }
    case "string": {
      node.type = StringType;
      break;
    }
    case "list type": {
      checkNodeTypes(node.elementType, context);
      node.type = new ListType(node.elementType.type);
      break;
    }
    case "map type": {
      checkNodeTypes(node.valueType, context);
      node.type = new MapType(node.valueType.type);
      break;
    }
    case "function type": {
      for (const parameter of node.parameters) {
        checkNodeTypes(parameter, context);
      }
      if (node.returnType) checkNodeTypes(node.returnType, context);
      node.type = new FunctionType(
        node.parameters.map((parameter) => parameter.type as NameAndType),
        node.returnType ? node.returnType.type : NothingType
      );
      break;
    }
    case "record type": {
      for (const field of node.fields) {
        checkNodeTypes(field, context);
      }
      node.type = new RecordType(node.fields.map((field) => new NameAndType(field.name.value, field.type)));
      break;
    }
    case "union type": {
      for (const type of node.unionTypes) {
        checkNodeTypes(type, context);
      }
      node.type = new UnionType(node.unionTypes.map((type) => type.type));
      break;
    }
    case "mixin type": {
      const seenFields = new Map<String, TypeSpecifierNode>();
      const fields = [];
      for (const type of node.mixinTypes) {
        checkNodeTypes(type, context);

        // Make sure the mixin type is a record
        if (!((type.type.kind == "named type" && type.type.type.kind == "record") || type.type.kind == "record")) {
          throw new LittleFootError(type.location, `All types in a mixin must be a record, but found '${type.type.signature}'.`);
        }

        // Make sure the fields of the record are unique within the mixin
        const record = type.type.kind == "named type" ? (type.type.type as RecordType) : (type.type as RecordType);
        for (const field of record.fields) {
          if (!seenFields.has(field.name)) {
            seenFields.set(field.name, type);
            fields.push(field);
          } else {
            const otherType = seenFields.get(field.name)!;
            const previousType = otherType.location.text;
            throw new LittleFootError(type.location, `Field '${field.name}' of mixin type already defined in previous mixin type '${previousType}'.`);
          }
        }
      }
      node.type = new RecordType(fields);
      break;
    }
    case "type reference": {
      if (!types.has(node.name.value)) {
        throw new LittleFootError(node.location, `Could not find type '${node.name.value}'.`);
      }
      const type = types.get(node.name.value)! as NamedType;
      // If we are in the type resolution phase, we might encounter types
      // that haven't been resolved yet in other type declarations. Resolve
      // them here.
      if (type.type == UnknownType) {
        checkNodeTypes(type.typeNode, context);
      }
      node.type = type;
      break;
    }
    case "type declaration": {
      // Recursively resolve the type. Also resolves other named types
      // via the "type reference" case above.
      if (node.type.kind == "named type") {
        const type = types.get(node.name.value)! as NamedType;
        type.type = ResolvingTypeMarker;
        checkNodeTypes(node.typeNode, context);
        if (node.typeNode.type == UnknownType) {
          throw new LittleFootError(node.name.location, `Internal compiler error: named type '${node.name.value}' should have a type set.`);
        }
        (node.type as NamedType).type = node.typeNode.type;
        // node.type = node.typeNode.type;
        return;
      }
      break;
    }
    case "name and type": {
      checkNodeTypes(node.typeNode, context);
      node.type = node.typeNode.type;
      break;
    }
    case "import":
      // FIXME implement imports
      throw new Error("Not implemented");
    case "imported name": {
      break; // no-op, handled in "import" case above
    }
    case "function declaration": {
      checkFunctionNode(node, context, true);
      break;
    }
    case "variable declaration": {
      checkNodeTypes(node.initializer, context);
      if (node.typeNode) {
        checkNodeTypes(node.typeNode, context);
        node.type = node.typeNode.type;
        if (!isAssignableTo(node.initializer, node.type)) {
          throw new LittleFootError(
            node.initializer.location,
            `Can not assign a '${node.initializer.type.signature}' to a '${node.type.signature}'.`
          );
        }
      } else {
        if (node.initializer.kind == "list literal" && node.initializer.elements.length == 0) {
          if (!node.initializer.typeNode) {
            throw new LittleFootError(node.name.location, "Can not assign an empty list without a type to a variable without a type.");
          }
        }
        if (node.initializer.kind == "map literal" && node.initializer.values.length == 0) {
          if (!node.initializer.typeNode) {
            throw new LittleFootError(node.name.location, "Can not assign an empty map to a variable without a type.");
          }
        }
        if (node.initializer.kind == "record literal" && hasEmptyListOrMap(node.initializer.type)) {
          throw new LittleFootError(node.name.location, "Can not assign a record with empty lists or maps to a variable without a type.");
        }
        node.type = node.initializer.type;
      }
      scopes.add(node);
      break;
    }
    case "if": {
      checkNodeTypes(node.condition, context);
      if (node.condition.type != BooleanType) {
        throw new LittleFootError(node.condition.location, `'if' condition must be a boolean but is a '${node.condition.type.signature}'.`);
      }

      // Gather the "is" operators and check if they are negated.
      const isOperators: { isNegated: boolean; isOperator: IsOperatorNode; oldType: Type }[] = [];
      let negated = false;
      traverseAst(node.condition, (node) => {
        if (node.kind == "unary operator" && node.operator.value == "not") negated = !negated;
        if (node.kind == "is operator") {
          isOperators.push({ isNegated: negated, isOperator: node, oldType: node.leftExpression.type });
        }
        return true;
      });

      // Temporarily set the types of each variable found in the "is" operators
      // to the type specified in the operator.
      // FIXME allow narrowing of MemberAccessNodes as well, instead of just VariableAccessNodes.
      for (const operator of isOperators) {
        if (operator.isOperator.leftExpression.kind != "variable access") {
          throw new LittleFootError(operator.isOperator.leftExpression.location, "Must be a variable.");
        }
        const variable = scopes.get((operator.isOperator.leftExpression as VariableAccessNode).name.value)!;
        const variableType = (operator.oldType = variable.type);
        if (variableType.kind != "union") {
          throw new LittleFootError(operator.isOperator.leftExpression.location, "Variable type must be a union.");
        }
        const narrowedType = operator.isOperator.typeNode.type;
        if (!isAssignableTo(operator.isOperator.typeNode, variable.type)) {
          throw new LittleFootError(
            variable.location,
            `Variable '${variable.name.value}' is a '${variable.type.signature}' and can never be a '${narrowedType.signature}'.`
          );
        }

        if (operator.isNegated) {
          const newTypes = variableType.types.filter((type) => !isAssignableTo(variable, narrowedType));
          if (newTypes.length == 0) {
            throw new LittleFootError(operator.isOperator.location, `Negation of 'is' operator results in empty type set.`); // TODO better message
          }
          variable.type = newTypes.length == 1 ? newTypes[0] : new UnionType(newTypes);
        } else {
          variable.type = narrowedType;
        }
      }

      scopes.push();
      for (const statement of node.trueBlock) {
        checkNodeTypes(statement, context);
      }
      scopes.pop();

      // Reset the variable types narrowed down in "is" operators
      for (const operator of isOperators) {
        const variable = scopes.get((operator.isOperator.leftExpression as VariableAccessNode).name.value)!;
        variable.type = operator.oldType;
      }

      scopes.push();
      for (const elseIf of node.elseIfs) {
        checkNodeTypes(elseIf, context);
      }
      scopes.pop();

      scopes.push();
      for (const statement of node.falseBlock) {
        checkNodeTypes(statement, context);
      }
      scopes.pop();

      node.type = NothingType;
      break;
    }
    case "while":
      checkNodeTypes(node.condition, context);
      if (node.condition.type != BooleanType) {
        throw new LittleFootError(node.condition.location, `'while' condition must be a boolean but is a '${node.condition.type.signature}'.`);
      }

      context.pushLoop(node);
      scopes.push();
      for (const statement of node.block) {
        checkNodeTypes(statement, context);
      }
      scopes.pop();
      context.popLoop();

      node.type = NothingType;
      break;
    case "loop variable":
      // Nothing to do here, type is assigned and checked in "for each" and "for" cases below
      break;
    case "for each":
      checkNodeTypes(node.list, context);
      if (node.list.type.kind != "list") {
        throw new LittleFootError(node.list.location, `'for each' needs a list but '${node.list.type.signature}' was given.`);
      }
      node.loopVariable.type = node.list.type.elementType;

      context.pushLoop(node);
      scopes.push();
      scopes.add(node.loopVariable);
      for (const statement of node.block) {
        checkNodeTypes(statement, context);
      }
      scopes.pop();

      node.type = NothingType;
      break;
    case "for":
      node.loopVariable.type = NumberType;
      checkNodeTypes(node.from, context);
      checkNodeTypes(node.to, context);
      if (node.step) checkNodeTypes(node.step, context);
      if (node.from.type != NumberType) {
        throw new LittleFootError(node.from.location, `'from' must be a number but is a '${node.from.type.signature}'.`);
      }
      if (node.to.type != NumberType) {
        throw new LittleFootError(node.from.location, `'to' must be a number but is a '${node.to.type.signature}'.`);
      }
      if (node.step && node.step.type != NumberType) {
        throw new LittleFootError(node.from.location, `'step' must be a number but is a '${node.step.type.signature}'.`);
      }

      context.pushLoop(node);
      scopes.push();
      scopes.add(node.loopVariable);
      for (const statement of node.block) {
        checkNodeTypes(statement, context);
      }
      scopes.pop();
      context.popLoop();

      node.type = NothingType;
      break;
    case "do":
      checkNodeTypes(node.condition, context);
      if (node.condition.type != BooleanType) {
        throw new LittleFootError(node.condition.location, `'do' condition must be a boolean but is a '${node.condition.type.signature}'.`);
      }

      context.pushLoop(node);
      scopes.push();
      for (const statement of node.block) {
        checkNodeTypes(statement, context);
      }
      scopes.pop();
      context.popLoop();

      node.type = NothingType;
      break;
    case "continue":
      if (!context.getCurentLoop()) {
        throw new LittleFootError(node.location, `'continue' can not be used outside a for, for each, while, or do loop.`);
      }
      node.type = NothingType;
      break;
    case "break":
      if (!context.getCurentLoop()) {
        throw new LittleFootError(node.location, `'break' can not be used outside a for, for each, while, or do loop.`);
      }
      node.type = NothingType;
      break;
    case "return":
      if (node.expression) {
        checkNodeTypes(node.expression, context);
        node.type = node.expression.type;
      } else {
        node.type = NothingType;
      }
      break;
    case "ternary operator":
      checkNodeTypes(node.condition, context);
      checkNodeTypes(node.trueExpression, context);
      checkNodeTypes(node.falseExpression, context);
      if (node.condition.type != BooleanType) {
        throw new LittleFootError(
          node.condition.location,
          `Ternary operator ? condition must be a boolean but is a '${node.condition.type.signature}'.`
        );
      }

      if (isEqual(node.trueExpression.type, node.falseExpression.type)) {
        node.type = node.trueExpression.type;
      } else {
        node.type = new UnionType([node.trueExpression.type, node.falseExpression.type]);
      }
      break;
    case "binary operator":
      checkNodeTypes(node.leftExpression, context);
      checkNodeTypes(node.rightExpression, context);
      switch (node.operator.value) {
        case "=":
          if (node.leftExpression.kind == "variable access") {
            const symbol = scopes.get(node.leftExpression.name.value);
            if (!symbol) {
              throw new LittleFootError(node.leftExpression.name.location, `Could not find variable '${node.leftExpression.name.value}'.`);
            }
            if (!isAssignableTo(node.rightExpression, node.leftExpression.type)) {
              throw new LittleFootError(
                node.rightExpression.location,
                `Can not assign a '${node.rightExpression.type.signature}' to a '${node.leftExpression.type.signature}'`
              );
            }
          } else if (node.leftExpression.kind == "member access") {
            if (!isAssignableTo(node.rightExpression, node.leftExpression.type)) {
              throw new LittleFootError(
                node.rightExpression.location,
                `Can not assign a '${node.rightExpression.type.signature}' to a '${node.leftExpression.type.signature}'`
              );
            }
          } else if (node.leftExpression.kind == "map or list access") {
            if (node.leftExpression.target.type.kind == "list") {
              if (!isAssignableTo(node.rightExpression, node.leftExpression.target.type.elementType)) {
                `Can not assign a '${node.rightExpression.type.signature}' to an array with '${node.leftExpression.target.type.elementType.signature}'`;
              }
            } else if (node.leftExpression.target.type.kind == "map") {
              if (!isAssignableTo(node.rightExpression, node.leftExpression.target.type.valueType)) {
                `Can not assign a '${node.rightExpression.type.signature}' to an array with '${node.leftExpression.target.type.valueType.signature}'`;
              }
            } else {
              throw new LittleFootError(node.leftExpression.target.location, `Can not use [] operator on a '${node.leftExpression.target.type}'.`);
            }
          } else {
            throw new LittleFootError(node.leftExpression.location, "Left side of assignment must be a variable, record field, list, or map.");
          }
          node.type = node.leftExpression.type;
          break;
        case "or":
        case "and":
        case "xor":
          if (node.leftExpression.type != BooleanType) {
            throw new LittleFootError(
              node.leftExpression.location,
              `Left operand of '${node.operator.value}' operator must be a boolean, but is a '${node.leftExpression.type.signature}'.`
            );
          }
          if (node.rightExpression.type != BooleanType) {
            throw new LittleFootError(
              node.rightExpression.location,
              `Left operand of '${node.operator.value}' operator must be a boolean, but is a '${node.rightExpression.type.signature}'.`
            );
          }
          node.type = BooleanType;
          break;
        case "==":
        case "!=":
          if (!isEqual(node.leftExpression.type, node.rightExpression.type)) {
            throw new LittleFootError(
              node.location,
              `Operands of '${node.operator.value}' operator must have the same type, but are '${node.leftExpression.type.signature}' and '${node.rightExpression.type.signature}'.`
            );
          }
          node.type = BooleanType;
          break;
        case "<":
        case "<=":
        case ">":
        case ">=":
          if (node.leftExpression.type != NumberType) {
            throw new LittleFootError(
              node.leftExpression.location,
              `Left operand of '${node.operator.value}' operator must be a number, but is a '${node.leftExpression.type.signature}'.`
            );
          }
          if (node.rightExpression.type != NumberType) {
            throw new LittleFootError(
              node.rightExpression.location,
              `Left operand of '${node.operator.value}' operator must be a number, but is a '${node.rightExpression.type.signature}'.`
            );
          }
          node.type = BooleanType;
          break;
        case "+":
        case "-":
        case "/":
        case "*":
        case "%":
          if (node.leftExpression.type != NumberType) {
            throw new LittleFootError(
              node.leftExpression.location,
              `Left operand of '${node.operator.value}' operator must be a number, but is a '${node.leftExpression.type.signature}'.`
            );
          }
          if (node.rightExpression.type != NumberType) {
            throw new LittleFootError(
              node.rightExpression.location,
              `Left operand of '${node.operator.value}' operator must be a number, but is a '${node.rightExpression.type.signature}'.`
            );
          }
          node.type = NumberType;
          break;
        default:
          throw new LittleFootError(node.operator.location, `Unknown operator ${node.operator.value}`);
      }
      break;
    case "unary operator":
      checkNodeTypes(node.expression, context);
      switch (node.operator.value) {
        case "not":
          if (node.expression.type != BooleanType) {
            throw new LittleFootError(
              node.expression.location,
              `Operand of 'not' operator must be a boolean, but is a '${node.expression.type.signature}'`
            );
          }
          node.type = BooleanType;
          break;
        case "+":
        case "-":
          if (node.expression.type != NumberType) {
            throw new LittleFootError(
              node.expression.location,
              `Operand of '${node.operator.value}' operator must be a number, but is a '${node.expression.type.signature}'`
            );
          }
          node.type = BooleanType;
          break;
        default:
          throw new LittleFootError(node.operator.location, `Unknown operator ${node.operator.value}`);
      }
      break;
    case "is operator":
      // Type checking of the is operator including narrowing is
      // done in the "if" case above.
      checkNodeTypes(node.leftExpression, context);
      checkNodeTypes(node.typeNode, context);
      node.type = BooleanType;
      break;
    case "list literal": {
      const seenSignatures = new Set<string>();
      const elementTypes: Type[] = [];
      for (const element of node.elements) {
        checkNodeTypes(element, context);
        if (!seenSignatures.has(element.type.signature)) {
          seenSignatures.add(element.type.signature);
          elementTypes.push(element.type);
        }
      }
      if (node.typeNode) checkNodeTypes(node.typeNode, context);
      if (elementTypes.length == 0) {
        if (node.typeNode) node.type = new ListType(node.typeNode.type);
        else node.type = new ListType(UnknownType);
      } else {
        node.type = new ListType(elementTypes.length == 1 ? elementTypes[0] : new UnionType(elementTypes));
      }
      break;
    }
    case "map literal": {
      const seenSignatures = new Set<string>();
      const valueTypes: Type[] = [];
      for (const element of node.values) {
        checkNodeTypes(element, context);
        if (!seenSignatures.has(element.type.signature)) {
          seenSignatures.add(element.type.signature);
          valueTypes.push(element.type);
        }
      }
      if (node.typeNode) checkNodeTypes(node.typeNode, context);
      if (valueTypes.length == 0) {
        if (node.typeNode) node.type = new MapType(node.typeNode.type);
        else node.type = new MapType(UnknownType);
      } else {
        node.type = new MapType(valueTypes.length == 1 ? valueTypes[0] : new UnionType(valueTypes));
      }
      break;
    }
    case "record literal":
      const fields: NameAndType[] = [];
      for (let i = 0; i < node.fieldNames.length; i++) {
        const fieldName = node.fieldNames[i].value;
        const fieldValue = node.fieldValues[i];
        checkNodeTypes(fieldValue, context);
        fields.push(new NameAndType(fieldName, fieldValue.type));
      }
      node.type = new RecordType(fields);
      break;
    case "function literal":
      node.type = checkFunctionNode(node, context);
      break;
    case "variable access":
      const symbol = scopes.get(node.name.value);
      if (!symbol) {
        throw new LittleFootError(node.name.location, `Could not find variable '${node.name.value}'.`);
      }
      node.type = symbol.type;
      break;
    case "member access":
      checkNodeTypes(node.object, context);
      const type = node.object.type.kind == "named function" || node.object.type.kind == "named type" ? node.object.type.type : node.object.type;
      if (type.kind == "record") {
        let found = false;
        for (const field of type.fields) {
          if (field.name == node.member.value) {
            node.type = field.type;
            found = true;
            break;
          }
        }
        if (!found) {
          throw new LittleFootError(node.member.location, `Field '${node.member.value}' does not exist on a '${node.object.type.signature}'.`);
        }
      } else if (type.kind == "list" || type.kind == "map" || type == StringType) {
        // FIXME map also has keys:[string] and values:[valueType]
        if (node.member.value !== "length") {
          throw new LittleFootError(node.member.location, `Field '${node.member.value}' does not exist on a '${node.object.type.signature}'.`);
        }
        node.type = NumberType;
      } else {
        throw new LittleFootError(node.member.location, `Field '${node.member.value}' does not exist on a '${node.object.type.signature}'.`);
      }
      break;
    case "map or list access":
      checkNodeTypes(node.keyOrIndex, context);
      checkNodeTypes(node.target, context);
      if (node.target.type.kind == "list") {
        if (node.keyOrIndex.type != NumberType) {
          throw new LittleFootError(node.keyOrIndex.location, `Index into list must be a number, but found a '${node.keyOrIndex.type.signature}'.`);
        }
        node.type = node.target.type.elementType;
      } else if (node.target.type.kind == "map") {
        if (node.keyOrIndex.type != StringType) {
          throw new LittleFootError(node.keyOrIndex.location, `Index into map must be a string, but found a '${node.keyOrIndex.type.signature}'.`);
        }
        node.type = node.target.type.valueType;
      } else {
        throw new LittleFootError(
          node.target.location,
          `The '[]' operator can only be used with lists or maps, but was used with a ${node.target.type.signature}.`
        );
      }
      break;
    case "function call":
      for (const arg of node.args) {
        checkNodeTypes(arg, context);
      }
      if (node.target.kind == "variable access") {
        // if the target is a variable name, first check if
        // there's one in the scope. If so, check its type
        const symbol = scopes.get(node.target.name.value);
        if (symbol) {
          if (symbol.type.kind != "function") {
            throw new LittleFootError(node.target.name.location, `'${node.target.name.value}' is not a function.`);
          }
          const functionType = symbol.type;
          if (functionType.parameters.length != node.args.length) {
            throw new LittleFootError(node.location, `Expected ${functionType.parameters.length} arguments, got ${node.args.length}.`);
          }
          for (let i = 0; i < node.args.length; i++) {
            const arg = node.args[i];
            const param = functionType.parameters[i];
            if (!isAssignableTo(arg, param.type)) {
              throw new LittleFootError(arg.location, `Expected a ${param.type.signature}, got a ${arg.type.signature}`);
            }
          }
          checkNodeTypes(node.target, context);
          node.type = functionType.returnType;
        } else {
          // Otherwise, lookup the best fitting function for the given args.
          const closestFunc = getClosestFunction(context, node.target.name.value, node.args);
          if (!closestFunc) {
            throw new LittleFootError(
              node.location,
              `Could not find function '${node.target.name.value}(${node.args.map((arg) => arg.type.signature).join(",")})'. ${
                functions.get(node.target.name.value)
                  ? "Candidates: \n" +
                    functions
                      .get(node.target.name.value)!
                      .map((func) => "\t" + func.signature)
                      .join("\n")
                  : ""
              }`
            );
          }
          if (closestFunc.length > 1) {
            throw new LittleFootError(
              node.location,
              `More than one function called '${node.target.name.value}' matches the arguments. Candidates: \n${closestFunc
                .map((func) => "\t" + func.signature)
                .join("\n")}`
            );
          }
          node.target.type = closestFunc[0].type;
          node.type = closestFunc[0].type.returnType;
        }
      } else {
        checkNodeTypes(node.target, context);
        if (node.target.type.kind != "function") {
          throw new LittleFootError(node.target.location, `Target of function call is not a function but a ${node.target.type.signature}.`);
        }
        const functionType = node.target.type;
        if (functionType.parameters.length != node.args.length) {
          throw new LittleFootError(node.location, `Expected ${functionType.parameters.length} arguments, got ${node.args.length}.`);
        }
        for (let i = 0; i < node.args.length; i++) {
          const arg = node.args[i];
          const param = functionType.parameters[i];
          if (!isAssignableTo(arg, param.type)) {
            throw new LittleFootError(arg.location, `Expected a ${param.type.signature}, got a ${arg.type.signature}`);
          }
        }
        node.type = functionType.returnType;
      }
      break;
    case "method call":
      for (const arg of node.args) {
        checkNodeTypes(arg, context);
      }
      checkNodeTypes(node.target.object, context);
      if (node.target.object.type.kind == "record") {
        const field = node.target.object.type.fields.find((field) => field.name == node.target.member.value);
        if (field) {
          if (field.type.kind != "function") {
            throw new LittleFootError(node.target.member.location, `'${node.target.member.value}' is not a function.`);
          }
          const functionType = field.type;
          if (functionType.parameters.length != node.args.length) {
            throw new LittleFootError(node.location, `Expected ${functionType.parameters.length} arguments, got ${node.args.length}.`);
          }
          for (let i = 0; i < node.args.length; i++) {
            const arg = node.args[i];
            const param = functionType.parameters[i];
            if (!isAssignableTo(arg, param.type)) {
              throw new LittleFootError(arg.location, `Expected a ${param.type.signature}, got a ${arg.type.signature}`);
            }
          }
          node.target.type = functionType;
          node.type = functionType.returnType;
          return;
        }
        // Fall through if no member function was found
      }
      // Otherwise, lookup the best fitting function for the given args, including the "object"
      // as the first argument.
      const args = [node.target.object, ...node.args];
      const closestFunc = getClosestFunction(context, node.target.member.value, args);
      if (!closestFunc) {
        throw new LittleFootError(
          node.location,
          `Could not find function '${node.target.member.value}(${args.map((arg) => arg.type.signature).join(",")})'. Candidates: \n${functions
            .get(node.target.member.value)
            ?.map((func) => "\t" + func.signature)
            .join("\n")}`
        );
      }
      if (closestFunc.length > 1) {
        throw new LittleFootError(
          node.location,
          `More than one function called '${node.target.member.value}' matches the arguments. Candidates: \n${closestFunc
            .map((func) => "\t" + func.signature)
            .join("\n")}`
        );
      }
      node.target.type = closestFunc[0].type;
      node.type = closestFunc[0].type.returnType;
      break;
    default:
      assertNever(node);
  }
}

function checkFunctionNode(node: FunctionLiteralNode | FunctionNode, context: TypeCheckerContext, checkCode = true) {
  context.scopes.push();
  for (const parameter of node.parameters) {
    checkNodeTypes(parameter, context);
    context.scopes.add(parameter);
  }

  if (node.returnType) checkNodeTypes(node.returnType, context);

  if (checkCode) {
    for (const statement of node.code) {
      checkNodeTypes(statement, context);
    }
  }
  context.scopes.pop();

  if (checkCode) {
    // If a return type was given, check that the returned expressions are assignable to it.
    // FIXME we actually need a CFG here. If one exit path doesn't
    // return anything, we need to check nothing against the returned type.
    if (node.returnType) {
      const returnType = node.returnType.type;
      for (const statement of node.code) {
        traverseAst(statement, (node) => {
          if (node.kind == "return") {
            if (!isAssignableTo(node, returnType)) {
              throw new LittleFootError(
                node.location,
                `Can not return a value of type '${node.type.signature}' from a function with return type '${returnType.signature}'.`
              );
            }
          }
          return true;
        });
      }
      const functionType = new FunctionType(
        node.parameters.map((parameter) => new NameAndType(parameter.name.value, parameter.type)),
        node.returnType.type
      );
      node.type = functionType;
      return functionType;
    } else {
      // Otherwise gather the types and infere the return type.
      // FIXME we actually need a CFG here. If one exit path doesn't
      // return anything, we need to add nothing to the infered type
      const returns: ReturnNode[] = [];
      for (const statement of node.code) {
        traverseAst(statement, (node) => {
          if (node.kind == "return") {
            returns.push(node);
          }
          return true;
        });
      }
      const returnTypes = returns.map((ret) => (ret.type.kind == "named function" || ret.type.kind == "named type" ? ret.type.type : ret.type));
      if (returnTypes.length == 0) returnTypes.push(NothingType);
      const functionType = new FunctionType(
        node.parameters.map((parameter) => new NameAndType(parameter.name.value, parameter.type)),
        returnTypes.length == 1 ? returnTypes[0] : new UnionType(returnTypes)
      );
      node.type = functionType;
      return functionType;
    }
  } else {
    return new FunctionType(
      node.parameters.map((parameter) => new NameAndType(parameter.name.value, parameter.type)),
      UnknownType
    );
  }
}

/**
 * Inferres types for `from` if necessary based on `to`, then checks
 * if `from` is assignable to `to`.
 *
 * Type inference for `from` is necessary if `from` has empty list or map
 * literals, so the runtime can instantiate the correct types for the empty
 * lists and map literals. The types of these literals are inferred from `to`.
 */
function isAssignableTo(from: AstNode, to: Type): boolean {
  assignTypesToEmptyListAndMapLiterals(from, to);
  // expandLiteralValueTypesToUnions(from, to);
  if (!typeIsAssignableTo(from.type, to)) return false;
  return true;
}

// Finds empty lists and maps in literals and infers their type based on to
// `to` type.
function assignTypesToEmptyListAndMapLiterals(from: AstNode, to: Type): boolean {
  const originalTo = to;
  to = to.kind == "named type" || to.kind == "named function" ? to.type : to;
  if (hasEmptyListOrMap(from.type)) {
    // The from type has an empty list or map literal in it. We need to infer
    // its type if possible. The from type must be a (nested) list, map, or record at
    // this point, and to must have a corresponding type.

    if (from.kind == "list literal" && from.type.kind == "list") {
      if (to.kind == "list") {
        // If the from list literal is not nested and empty, it has type
        // UnknownType. Assign the to type.
        if (from.type.elementType.signature == UnknownType.signature) {
          // Using signature because scoreFunction copies types.
          from.type = originalTo;
          return true;
        } else {
          // Otherwise, the from list literal has a nested value with an
          // unknown type. Recursively check and resolve the unknown types
          // of empty list or map literal values.
          for (const element of from.elements) {
            if (!assignTypesToEmptyListAndMapLiterals(element, to.elementType)) return false;
          }
          from.type.setElementType(nodeListToType(from.elements));
          return true;
        }
      } else if (to.kind == "union") {
        // `to` can also be a union.
        const listTypes = to.types.filter((type) => type.kind == "list");
        if (listTypes.length == 0) {
          // If there's no list type in the union, the empty list type can not be inferred.
          throw new LittleFootError(from.location, `Can not infer type for empty list literal from target type ${originalTo.signature}`);
        } else if (listTypes.length == 1) {
          // If the union contains 1 list type, try to assign that as the empty list type
          return assignTypesToEmptyListAndMapLiterals(from, listTypes[0]);
        } else {
          // If there are > 1 list types, the type is undecideable
          throw new LittleFootError(
            from.location,
            `Can not infer type for empty list literal from target type ${originalTo.signature}. Candidates:\n${listTypes
              .map((type) => "\t" + type.signature)
              .join("\n")}`
          );
        }
      } else {
        return true;
      }
    } else if (from.kind == "map literal" && from.type.kind == "map") {
      if (to.kind == "map") {
        // If the from list literal is not nested and empty, it has type
        // UnknownType. Assign the to type.
        if (from.type.kind == "map" && from.type.valueType.signature == UnknownType.signature) {
          // Using signature because scoreFunction copies types.
          from.type = originalTo;
          return true;
        } else {
          // Otherwise, the from map literal has a nested value with an
          // unknown type. Recursively check and resolve the unknown types
          // of the values.
          for (const value of from.values) {
            if (!assignTypesToEmptyListAndMapLiterals(value, to.valueType)) return false;
          }

          // Update the map literal's type.
          from.type.setValueType(nodeListToType(from.values));
          return true;
        }
      } else if (to.kind == "union") {
        // `to` can also be a union.
        const mapTypes = to.types.filter((type) => type.kind == "map");
        if (mapTypes.length == 0) {
          // If there's no list type in the union, the empty list type can not be inferred.
          throw new LittleFootError(from.location, `Can not infer type for empty map literal from target type ${originalTo.signature}`);
        } else if (mapTypes.length == 1) {
          // If the union contains 1 list type, try to assign that as the empty list type
          return assignTypesToEmptyListAndMapLiterals(from, mapTypes[0]);
        } else {
          // If there are > 1 list types, the type is undecideable
          throw new LittleFootError(
            from.location,
            `Can not infer type for empty map literal from target type ${originalTo.signature}. Candidates:\n${mapTypes
              .map((type) => type.signature)
              .join("\n")}`
          );
        }
      } else {
        return true;
      }
    } else if (from.kind == "record literal" && to.kind == "record") {
      // If the from record has less fields than the to record
      // we can stop infering types. The from record can not be
      // assigned to the to record.
      if (from.fieldValues.length < to.fields.length) return false;

      // Otherwise, fix up empty lists and maps in the record's field
      // which will also resolve their respective unknown types.
      for (let i = 0; i < from.fieldValues.length; i++) {
        const fieldName = from.fieldNames[i];
        const fieldValue = from.fieldValues[i];
        let found = false;
        for (const toField of to.fields) {
          if (fieldName.value !== toField.name) continue;
          if (assignTypesToEmptyListAndMapLiterals(fieldValue, toField.type)) {
            found = true;
            break;
          }
        }
        if (!found) return false;
      }
      // Update the record literal's type, which should not have
      // any empty list and map literals with unknown type anymore.
      const type = from.type as RecordType;
      for (let i = 0; i < from.fieldValues.length; i++) {
        type.fields[i].type = from.fieldValues[i].type;
      }
      (from.type as RecordType).updateSignature();
      return true;
    } else {
      return false;
    }
  } else {
    return true;
  }
}

function getClosestFunction(context: TypeCheckerContext, name: string, args: AstNode[]) {
  const funcs = context.module.functions.get(name);
  if (!funcs) return null;

  // Score each function. scoreFunction will return a score of Number.MAX_VALUE
  // if the function isn't compatible with the arguments.
  const scoredFunctions: { score: number; func: NamedFunction }[] = [];
  for (const func of funcs) {
    if (func.type.returnType == UnknownType) {
      checkFunctionNode(func.ast, context, true);
      func.updateReturnType();
    }
    const score = scoreFunction(func, args);
    if (score != Number.MAX_VALUE) {
      scoredFunctions.push({ score, func });
    }
  }
  if (scoredFunctions.length == 0) return null;

  // Sort functions from highest to lowest score.
  scoredFunctions.sort((a, b) => b.score - a.score);

  // Filter the functions to end up with a list of
  // candidate functions that share the highest score.
  const bestScore = scoredFunctions[0].score;
  const candidates = scoredFunctions.filter((scoredFunc) => scoredFunc.score == bestScore).map((scoredFunc) => scoredFunc.func);

  // If we found a single candidate, type empty lists and expand literal value types to union types
  // in the arguments.
  if (candidates.length == 1) {
    const func = candidates[0];
    for (let i = 0; i < args.length; i++) {
      const param = func.type.parameters[i].type;
      const arg = args[i];
      isAssignableTo(arg, param);
    }
  }
  return candidates;
}

function scoreFunction(func: NamedFunction, args: AstNode[]) {
  if (func.type.parameters.length != args.length) return Number.MAX_VALUE;

  let match = true;
  let score = 0;
  const originalArgTypes: Type[] = [];
  for (const arg of args) {
    originalArgTypes.push(arg.type);
  }
  try {
    // For each argument, calculate a score:
    // 1. If the types match, add 2 to the score
    // 2. If the argument type is assignable to the
    //    parameter type, add 1 to the score
    //
    // This way functions with more direct type matches
    // between arguments and parameters will score higher
    // overall.
    for (let i = 0; i < args.length; i++) {
      const param = func.type.parameters[i].type;
      const arg = args[i];
      if (isEqual(arg.type, param)) {
        score += 2;
      } else {
        // Need to copy the argument node, as isAssignableTo
        // may modify its type hierarchy.
        arg.type = arg.type.copy();
        if (isAssignableTo(arg, param)) {
          score += 1;
        } else {
          match = false;
          break;
        }
      }
    }
  } catch (e) {
    // isAssignableTo above can throw in case the type
    // of an empty list can not be resolved.
    return Number.MAX_VALUE;
  } finally {
    for (let i = 0; i < args.length; i++) {
      args[i].type = originalArgTypes[i];
    }
  }
  if (!match) return Number.MAX_VALUE;
  return score;
}

function hasEmptyListOrMap(type: Type) {
  let found = false;
  traverseType(type, (type) => {
    if (type.kind == "list" && type.elementType.signature == UnknownType.signature) {
      // Using signature because scoreFunction copies types.
      found = true;
      return false;
    }
    if (type.kind == "map" && type.valueType.signature == UnknownType.signature) {
      // Using signature because scoreFunction copies types.
      found = true;
      return false;
    }
    return true;
  });
  return found;
}

function hasUnion(type: Type) {
  let found = false;
  traverseType(type, (type) => {
    if (type.kind == "union") {
      found = true;
      return false;
    }
    return true;
  });
  return found;
}

function unify(a: Type, union: UnionType) {
  const seenSignatures = new Set<string>();
  const unionTypes: Type[] = [];

  if (a.kind == "union") {
    for (const type of a.types) {
      if (!seenSignatures.has(type.signature)) {
        seenSignatures.add(type.signature);
        unionTypes.push(type);
      }
    }
  } else {
    seenSignatures.add(a.signature);
    unionTypes.push(a);
  }

  for (const type of union.types) {
    if (!seenSignatures.has(type.signature)) {
      seenSignatures.add(type.signature);
      unionTypes.push(type);
    }
  }
  return new UnionType(unionTypes.length == 0 ? [UnknownType] : unionTypes);
}

function nodeListToType(nodes: AstNode[]) {
  const seenSignatures = new Set<string>();
  const elementTypes: Type[] = [];
  for (const node of nodes) {
    if (!seenSignatures.has(node.type.signature)) {
      seenSignatures.add(node.type.signature);
      elementTypes.push(node.type);
    }
  }
  if (elementTypes.length == 0) {
    return UnknownType;
  } else {
    return elementTypes.length == 1 ? elementTypes[0] : new UnionType(elementTypes);
  }
}

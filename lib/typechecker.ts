import { AstNode, FunctionLiteralNode, FunctionNode, IsOperatorNode, TypeNode, TypeSpecifierNode, VariableAccessNode, traverseAst } from "./ast";
import { CompilerContext } from "./compiler";
import { LittleFootError } from "./error";
import { SourceLocation } from "./source";
// prettier-ignore
import { BooleanType, FunctionType, ListType, MapType, NameAndType, NamedFunction, NamedType, NothingType, NumberType, RecordType, ResolvingTypeMarker, StringType, Type, UnionType, UnknownType, isAssignableTo, isEqual } from "./types";

function assertNever(x: never) {
  throw new Error("Unexpected object: " + x);
}

export function checkTypes(ast: AstNode[], context: CompilerContext) {
  const { errors, types } = context;

  // Gather named type nodes. These are named types that other
  // types may refer to. Set their type to UnknownType.
  const namedTypes: TypeNode[] = ast.filter((node) => node.kind == "type declaration") as TypeNode[];
  for (const type of namedTypes) {
    if (types.has(type.name.value)) {
      errors.push(
        new LittleFootError(type.name.location, `Can not use '${type.name.value}' as a type name, as a built-in type with that name exists.`)
      );
    }
    type.type = new NamedType(type.name.value, UnknownType, type, type.location);
  }

  // Deduplicate named types. Can't use types.add()/has() yet, as the signature
  // is incomplete at this point.
  // FIXME this needs to take into account imported types. Need to check types of
  // import statements first or do the import stuff in the compiler and add
  // modules beforehand?
  const namedTypesLookup = new Map<String, TypeNode>();
  for (const type of namedTypes) {
    if (namedTypesLookup.has(type.name.value)) {
      const otherType = namedTypesLookup.get(type.name.value)!;
      const otherTypeLineIndex = otherType.location.source.indicesToLines(otherType.name.location.start, otherType.name.location.end)[0].index;
      errors.push(
        new LittleFootError(
          type.name.location,
          `Duplicate type '${type.name.value}', first defined in ${otherType.location.source.path}:${otherTypeLineIndex}.`
        )
      );
    } else {
      namedTypesLookup.set(type.name.value, type);
    }
  }
  if (errors.length > 0) return;

  // For each named type, replace the UnknownType with the real type. Detects circular types
  // in checkNodeTypes() case "type reference".
  for (const type of namedTypes) {
    types.add(type.type as NamedType);
  }
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

  // All named types are defined, assign and check the types of all AST nodes.
  for (const node of ast) {
    // FIXME recover in case a statement or expression throws an error and type check the remainder of the AST if possible
    // This should work for errors within functions. For top-level statements, stop if a var declaration fails.
    try {
      checkNodeTypes(node, context);
    } catch (e) {
      if (e instanceof LittleFootError) errors.push(e);
      else errors.push(new LittleFootError(node.location, "Internal error: " + (e as any).message + "\n" + (e as any).stack));
      return;
    }
  }

  // Final check that we have no unknown types in the AST.
  for (const node of ast) {
    traverseAst(node, (node) => {
      if (node.type == UnknownType) {
        throw new LittleFootError(node.location, "Internal error: AST node has unknown type.");
      }
      return true;
    });
  }
}

export function checkNodeTypes(node: AstNode, context: CompilerContext) {
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
      if (!context.types.has(node.name.value)) {
        throw new LittleFootError(node.location, `Could not find type '${node.name.value}'.`);
      }
      const type = context.types.get(node.name.value)! as NamedType;
      // If we are in the type resolution phase, we might encounter types
      // that haven't been resolved yet in other type declarations. Resolve
      // them here. Set a marker type the first time, so we can detect
      // circular types.
      if (type.type == UnknownType) {
        type.type = ResolvingTypeMarker;
        checkNodeTypes(type.typeNode, context);
      }
      // Found a circular type
      if (type.type == ResolvingTypeMarker) {
        throw new LittleFootError(type.location, `Type '${type.name}' circularly references itself.`);
      }
      node.type = type;
      break;
    }
    case "name and type": {
      checkNodeTypes(node.typeNode, context);
      node.type = node.typeNode.type;
      break;
    }
    case "import":
      throw new Error("Not implemented");
    case "imported name": {
      break; // no-op, handled in "import" case above
    }
    case "function declaration": {
      const functionType = checkFunctionNode(node, context);

      const namedFunction = new NamedFunction(node.name.value, functionType, node.code, node.exported, node.external, node.location);
      if (context.types.has(namedFunction.signature)) {
        const otherType = context.types.get(namedFunction.signature)! as NamedFunction;
        const otherTypeLineIndex = otherType.location.source.indicesToLines(otherType.location.start, otherType.location.end)[0].index;
        throw new LittleFootError(
          node.name.location,
          `Duplicate function '${node.name.value}', first defined in ${otherType.location.source.path}:${otherTypeLineIndex}.`
        );
      }

      // FIXME check if a function with the same name and signature was already defined.
      context.types.add(namedFunction);
      break;
    }
    case "type declaration": {
      // We set the type of type declaration nodes to new NamedType("name", UnknownType) in
      // checkTypes(). When we first resolve the type of a type declaration, we check the
      // type fo the right hand side of the assignement, replace the UnknownType in the
      // NamedType with it, then assign the plain type to the node itself.
      // All type declaration nodes are checked a second time as part of iterating over all statements
      // in the module AST in checkTypes(). We do not have to do anything anymore in that case, as the
      // type is fully defined and checked.
      if (node.type.kind == "named type") {
        checkNodeTypes(node.typeNode, context);
        if (node.typeNode.type == UnknownType) {
          throw new LittleFootError(node.name.location, `Internal compiler error: named type '${node.name.value}' should have a type set.`);
        }
        (node.type as NamedType).type = node.typeNode.type;
        node.type = node.typeNode.type;
      }
      break;
    }
    case "variable declaration": {
      checkNodeTypes(node.initializer, context);
      if (node.typeNode) {
        checkNodeTypes(node.typeNode, context);
        node.type = node.typeNode.type;
        if (!isAssignableTo(node.initializer.type, node.type)) {
          throw new LittleFootError(
            node.initializer.location,
            `Can not assign a '${node.initializer.type.signature}' to a '${node.type.signature}'.`
          );
        }
      } else {
        node.type = node.initializer.type;
      }
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
        if (node.kind == "unary operator" && node.operator.value == "!") negated = !negated;
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
          throw new LittleFootError(operator.isOperator.leftExpression.location, "Must be a variable."); // FIXME better error message.
        }
        const variable = operator.isOperator.leftExpression as VariableAccessNode;
        const variableType = (operator.oldType =
          variable.type.kind == "named function" || variable.type.kind == "named type" ? variable.type.type : variable.type);
        if (variableType.kind != "union") {
          throw new LittleFootError(operator.isOperator.leftExpression.location, "Variable type must be a union."); // FIXME better error mesage.
        }
        const narrowedType = operator.isOperator.typeNode.type;
        if (!isAssignableTo(variable.type, narrowedType)) {
          throw new LittleFootError(
            variable.location,
            `Variable '${variable.name.value}' is a '${variable.type.signature}' and can never be a '${narrowedType.signature}'.`
          );
        }

        if (operator.isNegated) {
          const newTypes = variableType.types.filter((type) => !isAssignableTo(type, narrowedType));
          variable.type = new UnionType(newTypes);
        } else {
          variable.type = narrowedType;
        }
      }

      for (const statement of node.trueBlock) {
        checkNodeTypes(statement, context);
      }

      // Reset the variable types narrowed down in "is" operators
      for (const operator of isOperators) {
        const variable = operator.isOperator.leftExpression as VariableAccessNode;
        variable.type = operator.oldType;
      }

      for (const elseIf of node.elseIfs) {
        checkNodeTypes(elseIf, context);
      }

      for (const statement of node.falseBlock) {
        checkNodeTypes(statement, context);
      }

      node.type = NothingType;
      break;
    }
    case "while":
      checkNodeTypes(node.condition, context);
      if (node.condition.type != BooleanType) {
        throw new LittleFootError(node.condition.location, `'while' condition must be a boolean but is a '${node.condition.type.signature}'.`);
      }

      for (const statement of node.block) {
        checkNodeTypes(statement, context);
      }

      node.type = NothingType;
      break;
    case "for each":
      checkNodeTypes(node.list, context);
      if (!(node.list.type.kind == "list")) {
        throw new LittleFootError(node.list.location, `'for each' needs a list but '${node.list.type.signature}' was given.`);
      }

      for (const statement of node.block) {
        checkNodeTypes(statement, context);
      }

      node.type = NothingType;
      break;
    case "for":
      checkNodeTypes(node.from, context);
      checkNodeTypes(node.to, context);
      if (node.step) checkNodeTypes(node.step, context);
      if (node.from.type != NumberType) {
        throw new LittleFootError(node.from.location, `'From' must be a number but is a '${node.from.type.signature}'.`);
      }
      if (node.to.type != NumberType) {
        throw new LittleFootError(node.from.location, `'To' must be a number but is a '${node.to.type.signature}'.`);
      }
      if (node.step && node.step.type != NumberType) {
        throw new LittleFootError(node.from.location, `'Step' must be a number but is a '${node.step.type.signature}'.`);
      }

      for (const statement of node.block) {
        checkNodeTypes(statement, context);
      }

      node.type = NothingType;
      break;
    case "do":
      checkNodeTypes(node.condition, context);
      if (node.condition.type != BooleanType) {
        throw new LittleFootError(node.condition.location, `'Do' condition must be a boolean but is a '${node.condition.type.signature}'.`);
      }

      for (const statement of node.block) {
        checkNodeTypes(statement, context);
      }

      node.type = NothingType;
      break;
    case "continue":
      // FIXME check if we are within a loop
      node.type = NothingType;
      break;
    case "break":
      // FIXME check if we are within a loop
      node.type = NothingType;
      break;
    case "return":
      if (node.expression) {
        checkNodeTypes(node.expression, context);
        node.type = node.expression.type;
      } else {
        node.type = NothingType;
      }
      // FIXME Check if the function's return type matches the expression
      // FIXME infer function return type?
      // FIXME check if we are inside a function (or not, as we are always inside a function
      // even for top-level statements which go into $moduleMain?)
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
            // FIXME implement scoped variable lookup
            throw Error("Assignment to variables not implemented");
          } else if (node.leftExpression.kind == "member access") {
            if (!isAssignableTo(node.rightExpression.type, node.leftExpression.type)) {
              throw new LittleFootError(
                node.rightExpression.location,
                `Can not assign a '${node.rightExpression.type.signature}' to a '${node.leftExpression.type.signature}'`
              );
            }
          } else if (node.leftExpression.kind == "map or list access") {
            if (node.leftExpression.target.type.kind == "list") {
              if (!isAssignableTo(node.rightExpression.type, node.leftExpression.target.type.elementType)) {
                `Can not assign a '${node.rightExpression.type.signature}' to an array with '${node.leftExpression.target.type.elementType.signature}'`;
              }
            }
          } else {
            throw new LittleFootError(node.leftExpression.location, "Left side of assignment must be a variable, record field, list, or map.");
          }
          node.type == node.leftExpression.type;
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
          if (isEqual(node.leftExpression.type, node.rightExpression.type)) {
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
          node.type = BooleanType;
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
      const types: Type[] = [];
      for (const element of node.elements) {
        checkNodeTypes(element, context);
        if (!seenSignatures.has(element.type.signature)) {
          seenSignatures.add(element.type.signature);
          types.push(element.type);
        }
      }
      if (types.length == 1) {
        node.type = types[0];
      } else {
        node.type = new UnionType(types);
      }
      break;
    }
    case "map literal": {
      const seenSignatures = new Set<string>();
      const types: Type[] = [];
      for (const element of node.values) {
        checkNodeTypes(element, context);
        if (!seenSignatures.has(element.type.signature)) {
          seenSignatures.add(element.type.signature);
          types.push(element.type);
        }
      }
      if (types.length == 1) {
        node.type = types[0];
      } else {
        node.type = new UnionType(types);
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
      throw Error("not implemented");
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
          `The '[]' operator can only be used with lists or maps, but found a ${node.target.type.signature}.`
        );
      }
      break;
    case "function call":
      throw Error("not implemented");
    case "method call":
      throw Error("not implemented");
    default:
      assertNever(node);
  }
}

function checkFunctionNode(node: FunctionLiteralNode | FunctionNode, context: CompilerContext) {
  for (const parameter of node.parameters) {
    checkNodeTypes(parameter, context);
  }

  if (node.returnType) checkNodeTypes(node.returnType, context);

  const functionType = new FunctionType(
    node.parameters.map((parameter) => new NameAndType(parameter.name.value, parameter.type)),
    node.returnType ? node.returnType.type : NothingType
  );

  for (const statement of node.code) {
    checkNodeTypes(statement, context);
  }

  node.type = functionType;
  return functionType;
}

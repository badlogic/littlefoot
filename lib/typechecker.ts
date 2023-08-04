// prettier-ignore
import { AstNode, BaseAstNode, DoNode, ExpressionNode, ForEachNode, ForNode, FunctionLiteralNode, FunctionNode, ImportNode, ImportedNameNode, IsOperatorNode, LoopVariable, NameAndTypeNode, NumericWideningNode, ReturnNode, StatementNode, TypeNode, TypeReferenceNode, TypeSpecifierNode, UnionExpansionNode, VariableAccessNode, VariableNode, WhileNode, traverseAst } from "./ast";
import { CompilerContext, Module, compileModule } from "./compiler";
import { LittleFootError, indent } from "./error";
import { SourceLocation } from "./source";
import { IdentifierToken } from "./tokenizer";
// prettier-ignore
import { AnyType, BooleanType, Float32Type, Float64Type, FunctionType, Int16Type, Int32Type, Int8Type, ListType, MapType, NameAndType, NamedFunctionType, NamedType, NothingType, NumberType, PrimitiveType, RecordType, ResolvingTypeMarker, StringType, Type, UnionType, UnknownType, hasEmptyListOrMap, hasUnion, isEqual, isGeneric, rawType, traverseType, isAssignableTo as typeIsAssignableTo } from "./types";

function assertNever(x: never) {
  throw new Error("Unexpected object: " + x);
}

export type Symbol = VariableNode | NameAndTypeNode | LoopVariable;

export const overloadableBinaryOperators = ["or", "and", "xor", "<", "<=", ">", ">=", "+", "-", "/", "*", "%", "[]"];

export class SymbolScopes {
  public scopes = new Array<Map<string, Symbol>>();

  constructor() {
    this.push();
  }

  push() {
    this.scopes.push(new Map<string, Symbol>());
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

  add(name: string, node: Symbol, allowShadow = true) {
    let scopes = this.scopes;
    for (var i = scopes.length - 1; i >= 0; i--) {
      let scope = scopes[i];
      let other = scope.get(name);
      if (other) {
        // Adding the exact same symbol is allowed so
        // module variable import handling is easier.
        if (other.location.equals(node.location)) {
          return;
        }
        throw new LittleFootError(node.name.location, `Duplicate variable ${name}, first defined in ${other.name.location.toString()}.`);
      }
      if (allowShadow) break;
    }
    scopes[scopes.length - 1].set(name, node);
  }
}

export class GenericBindings {
  scopes = new Array<Map<string, Type>>();

  constructor() {
    this.push();
  }

  push() {
    const bindings = new Map<string, Type>();
    this.scopes.push(bindings);
    return bindings;
  }

  pop() {
    if (this.scopes.length == 0) throw new Error("Tried to pop generic bindings without there being any.");
    this.scopes.pop();
  }

  get(name: string): Type | undefined {
    let scopes = this.scopes;
    for (var i = scopes.length - 1; i >= 0; i--) {
      let scope = scopes[i];
      let type = scope.get(name);
      if (type) {
        return type;
      }
    }
    return undefined;
  }

  add(name: string, binding: Type, location: SourceLocation, allowShadow = true) {
    let scopes = this.scopes;
    for (var i = scopes.length - 1; i >= 0; i--) {
      let scope = scopes[i];
      let other = scope.get(name);
      if (other) {
        throw new LittleFootError(location, `Duplicate generic binding ${name}.`);
      }
      if (allowShadow) break;
    }
    scopes[scopes.length - 1].set(name, binding);
  }
}

export class TypeCheckerContext {
  constructor(
    public readonly module: Module,
    public readonly compilerContext: CompilerContext,
    private readonly scopes = new SymbolScopes(),
    private readonly genericBindings = new GenericBindings(),
    private readonly currentLoop: (ForNode | ForEachNode | WhileNode | DoNode)[] = [],
    private currentFunctionOrType: (FunctionNode | TypeNode)[] = []
  ) {}

  withScope(f: () => void) {
    this.scopes.push();
    try {
      f();
    } finally {
      this.scopes.pop();
    }
  }

  withModuleScope(f: () => void) {
    const fullScopes = this.scopes.scopes;
    this.scopes.scopes = [fullScopes[0]];
    this.scopes.push();

    try {
      f();
    } finally {
      this.scopes.scopes = fullScopes;
    }
  }

  addToScope(name: string, node: Symbol, allowShadow = true) {
    this.scopes.add(name, node, allowShadow);
  }

  getFromScope(name: string) {
    return this.scopes.get(name);
  }

  withGenericBindings(f: () => void) {
    this.genericBindings.push();
    try {
      f();
    } finally {
      this.genericBindings.pop();
    }
  }

  addGenericBinding(name: string, binding: Type, location: SourceLocation, allowShadow = true) {
    this.genericBindings.add(name, binding, location, allowShadow);
  }

  getGenericBinding(name: string) {
    return this.genericBindings.get(name);
  }

  withLoop(loop: ForNode | ForEachNode | WhileNode | DoNode, f: () => void) {
    this.currentLoop.push(loop);
    try {
      f();
    } finally {
      this.currentLoop.pop();
    }
  }

  getCurentLoop(): ForNode | ForEachNode | WhileNode | DoNode | undefined {
    if (this.currentLoop.length == 0) return undefined;
    return this.currentLoop[this.currentLoop.length - 1];
  }

  withCurrentFunctionOrType(node: FunctionNode | TypeNode, f: () => void) {
    this.currentFunctionOrType.push(node);
    try {
      f();
    } finally {
      this.currentFunctionOrType.pop();
    }
  }

  getCurrentFunctionOrType() {
    if (this.currentFunctionOrType.length == 0) return undefined;
    return this.currentFunctionOrType[this.currentFunctionOrType.length - 1];
  }

  isInGenericFunctionOrTypeDeclaration(ignoreInstantiated = false) {
    if (this.currentFunctionOrType.length == 0) return false;
    const current = this.currentFunctionOrType[this.currentFunctionOrType.length - 1];
    if (!current) return false;
    if (current.genericTypeNames.length == 0) return false;
    if (current.type == UnknownType) return true; // Happens when func is first checked in checkTypes()
    if ((current.type.kind == "named function" || current.type.kind == "named type") && (ignoreInstantiated || !current.type.isInstantiated))
      return true;
    return false;
  }
}

function newGenericAnyType(name: IdentifierToken): NamedType {
  const typeNode = new TypeNode(name, name, [], new TypeReferenceNode(name, [], null), false);
  typeNode.type = AnyType;
  typeNode.typeNode.type = AnyType;
  return new NamedType(name.value, [], true, AnyType, typeNode, false, name.location);
}

export function checkTypes(context: TypeCheckerContext) {
  const { ast, types, functions } = context.module;
  const errors = context.compilerContext.errors;

  // Extract all top level statements into a generated $main function.
  const mainStatements = context.module.ast.filter((node) => {
    return node.kind != "import" && node.kind != "function declaration" && node.kind != "type declaration";
  }) as StatementNode[];
  const mainLocation = new SourceLocation(context.module.source, 0, context.module.source.text.length);
  const mainNode = new FunctionNode(mainLocation, new IdentifierToken(mainLocation, "$main"), [], [], null, mainStatements, true, false);
  const mainType = new NamedFunctionType("$main", [], true, new FunctionType([], NothingType), mainNode, false, false, mainLocation);
  mainNode.type = mainType;
  context.module.functions.add("$main", mainType);

  // Handle all the imports, also import stdlib
  importModule(context.compilerContext.modules.get("stdlib.lf")!, context.module, context);
  const imports: ImportNode[] = ast.filter((node) => node.kind == "import") as ImportNode[];
  for (const imp of imports) {
    try {
      checkNodeTypes(imp, context);
    } catch (e) {
      if (e instanceof LittleFootError) errors.push(e);
      else
        errors.push(
          new LittleFootError(new SourceLocation(imp.location.source, 0, 1), "Internal error: " + (e as any).message + "\n" + (e as any).stack)
        );
      return;
    }
  }

  // Gather all module level vars after resolving imports,
  // as imports will set imported module level vars.
  context.module.ast.forEach((node) => {
    if (node.kind == "variable declaration") {
      context.module.variables.set(node.name.value, node);
      node.moduleVariable = true;
    }
  });

  // Gather named type nodes. These are named types that other
  // types may refer to. Set their type to UnknownType.
  const namedTypeNodes: TypeNode[] = ast.filter((node) => node.kind == "type declaration") as TypeNode[];
  const namedTypes = new Array<NamedType>();
  for (const typeNode of namedTypeNodes) {
    try {
      typeNode.type = new NamedType(
        typeNode.name.value,
        typeNode.genericTypeNames.map((name) => new NameAndType(name.value, newGenericAnyType(name))),
        typeNode.genericTypeNames.length == 0,
        UnknownType,
        typeNode,
        typeNode.exported,
        typeNode.location
      );
      const namedType = typeNode.type as NamedType;
      types.add(namedType.name, namedType);
      namedTypes.push(typeNode.type as NamedType);
    } catch (e) {
      if (e instanceof LittleFootError) errors.push(e);
      else
        errors.push(
          new LittleFootError(new SourceLocation(typeNode.location.source, 0, 1), "Internal error: " + (e as any).message + "\n" + (e as any).stack)
        );
      return;
    }
  }

  // For each named type, replace their UnknownType with the real type by
  // recursive type resolution in checkNodeTypes() cases "type reference" and
  // "type declaration".
  for (const type of namedTypeNodes) {
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

  // Add constructor functions for each named type to module. Instantiate generic types.
  for (let i = 0; i < namedTypes.length; i++) {
    const typeNode = namedTypeNodes[i];
    let type = namedTypes[i];

    if (type.genericTypes.length > 0 && isGeneric(type)) {
      // Make sure the generic type bindings are resolved and generate the bindings
      // to be used by instantiation.
      const bindings = new Map<string, Type>();
      for (const genericTypeName of typeNode.genericTypeNames) {
        const genericType = (typeNode.type as NamedType).getGenericType(genericTypeName.value);
        bindings.set(genericTypeName.value, genericType);
      }

      // Instantiate the type
      try {
        context.withCurrentFunctionOrType(typeNode, () => {
          namedTypes[i] = typeNode.type = type = instantiateGenericType(typeNode, type, bindings, context) as NamedType;
        });
      } catch (e) {
        if (e instanceof LittleFootError) errors.push(e);
        else errors.push(new LittleFootError(typeNode.location, "Internal error: " + (e as any).message + "\n" + (e as any).stack));
      }
    }

    let actualType = rawType(type);
    if (actualType.kind == "record") {
      // Generate constructor function for named record.
      const funcType = new FunctionType(actualType.fields, type);
      const genericBindingNodes = typeNode.genericTypeNames.map((name) => new TypeReferenceNode(name, [], null));
      for (const genericBindingNode of genericBindingNodes) {
        genericBindingNode.type = type.getGenericType(genericBindingNode.name.value);
      }
      const returnTypeNode = new TypeReferenceNode(typeNode.name, genericBindingNodes, null);
      returnTypeNode.type = type;
      type.constructorFunction = new NamedFunctionType(
        type.name,
        typeNode.genericTypeNames.map((name) => new NameAndType(name.value, type.getGenericType(name.value))),
        typeNode.genericTypeNames.length == 0,
        funcType,
        new FunctionNode(typeNode.location, typeNode.name, typeNode.genericTypeNames, [], returnTypeNode, [], typeNode.exported, true, false),
        true,
        typeNode.exported,
        typeNode.name.location
      );
      type.constructorFunction.ast.type = type.constructorFunction;
      functions.add(type.name, type.constructorFunction);
    }
  }
  if (errors.length > 0) return;

  // Gather named functions and assign their parameter types. The return type is
  // assigned lazily once the function is encountered during AST traversal below,
  // through a second call to `checkFunctionNode()`.
  const namedFunctions = ast.filter((node) => node.kind == "function declaration") as FunctionNode[];
  for (const func of namedFunctions) {
    try {
      context.withCurrentFunctionOrType(func, () => {
        const namedFunction = checkFunctionDeclarationNode(func, context, false);
        functions.add(namedFunction.name, namedFunction);
      });
    } catch (e) {
      if (e instanceof LittleFootError) errors.push(e);
      else errors.push(new LittleFootError(func.location, "Internal error: " + (e as any).message + "\n" + (e as any).stack));
    }
  }
  if (errors.length > 0) return;

  // All named types are defined, assign and check the types of all AST nodes. This will also add all named functions
  // to module.functions, see the "function declaration" case in checkNodeTypes().
  for (const node of ast) {
    // Skip imports, they are fully handled above
    if (node.kind == "import") continue;
    // Skip type declarations, they are fully handled above
    if (node.kind == "type declaration") continue;

    // TODO recover in case a statement or expression throws an error and type check the remainder of the AST if possible
    // This should work for errors within functions. For top-level statements, stop if a var declaration fails.
    // Make sure that state in TypeCheckerContext is cleaned-up properly, e.g. current loop, scopes, generic bindings, etc.
    try {
      checkNodeTypes(node, context);
    } catch (e) {
      if (e instanceof LittleFootError) errors.push(e);
      else errors.push(new LittleFootError(node.location, "Internal error: " + (e as any).message + "\n" + (e as any).stack));
    }
  }
  if (errors.length > 0) return;

  // Finally check that we have no unknown and named types in the AST.
  for (const node of ast) {
    // Ignore type and function declaration nodes
    if (node.kind == "type declaration") continue;
    try {
      traverseAst(node, null, (node) => {
        traverseType(node.type, (type) => {
          if (type == UnknownType) {
            throw new LittleFootError(node.location, "Internal error: AST node has unknown type.");
          }
          return true;
        });
        return true;
      });
    } catch (e) {
      if (e instanceof LittleFootError) errors.push(e);
      else errors.push(new LittleFootError(node.location, "Internal error: " + (e as any).message + "\n" + (e as any).stack));
    }
  }
  if (errors.length > 0) return;

  // Also check all functions for unknown types
  functions.lookup.forEach((funcs) => {
    for (const func of funcs) {
      try {
        traverseType(func.type, (type) => {
          if (type.kind == "named type") return false;
          if (type == UnknownType) {
            throw new LittleFootError(func.location, `Internal error: named function ${func.signature} has unknown type.`);
          }
          return true;
        });
      } catch (e) {
        if (e instanceof LittleFootError) errors.push(e);
        else errors.push(new LittleFootError(func.location, "Internal error: " + (e as any).message + "\n" + (e as any).stack));
      }
    }
  });
}

export function checkNodeTypes(node: AstNode, context: TypeCheckerContext) {
  const types = context.module.types;

  switch (node.kind) {
    case "nothing literal": {
      node.type = NothingType;
      break;
    }
    case "boolean literal": {
      node.type = BooleanType;
      break;
    }
    case "number literal": {
      node.type = NumberType;
      break;
    }
    case "string literal": {
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
        node.parameters.map((parameter) => new NameAndType(parameter.name.value, parameter.type)),
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
      let unionType = new UnionType(node.unionTypes.map((type) => type.type));
      unionType = unify(unionType.types[0], unionType);
      node.type = unionType.types.length == 1 ? unionType.types[0] : unionType;
      break;
    }
    case "mixin type": {
      const seenFields = new Map<string, TypeSpecifierNode>();
      const fields = [];
      for (const type of node.mixinTypes) {
        checkNodeTypes(type, context);

        // Make sure the mixin type is a record
        if (!(rawType(type.type).kind == "record")) {
          throw new LittleFootError(type.location, `All types in a mixin must be a record, but found '${type.type.signature}'.`);
        }

        // Make sure the fields of the record are unique within the mixin
        const record = rawType(type.type) as RecordType;
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
      if (!types.has(node.name.value) && !context.getGenericBinding(node.name.value)) {
        throw new LittleFootError(node.location, `Can not find type '${node.name.value}'.`);
      }
      // Look for the type in the generic bindings first.
      let type = context.getGenericBinding(node.name.value) ? context.getGenericBinding(node.name.value)! : types.get(node.name.value)!;

      // If the reference is a primitive type, return early
      if (type.kind == "primitive") {
        node.type = type;
        return;
      } else {
        // If this is a named type, make sure it's resolved and instantiate it
        // if it has generic bindings
        if (type.kind == "named type") {
          // If we are in the type resolution phase, we might encounter types
          // that haven't been resolved yet in other type declarations. Resolve
          // them here.
          if (type.type == UnknownType) {
            checkNodeTypes(type.ast, context);
          }

          // If generic bindings were given, but the type isn't generic, report an error
          if (node.genericTypeBindings.length > 0 && type.genericTypes.length == 0) {
            throw new LittleFootError(node.location, `Type ${node.name.value} is not a generic type, but generic type arguments were given.`);
          }

          // Number of bindings and number of generic types must match
          if (node.genericTypeBindings.length != type.genericTypes.length) {
            throw new LittleFootError(
              node.location,
              `Wrong number of generic type parameters given, expected ${type.genericTypes.length}, got ${node.genericTypeBindings.length}`
            );
          }

          // If this reference is generic and has bindings, instantiate the new type.
          if (type.type != ResolvingTypeMarker && type.genericTypes.length > 0 && isGeneric(type)) {
            // Make sure the generic type bindings are resolved and generate the bindings
            // to be used by instantiation.
            const bindings = new Map<string, Type>();
            for (let i = 0; i < node.genericTypeBindings.length; i++) {
              const genericTypeBinding = node.genericTypeBindings[i];
              checkNodeTypes(genericTypeBinding, context);
              bindings.set(type.genericTypes[i].name, genericTypeBinding.type);
            }

            // Instantiate the type
            type = instantiateGenericType(node, type, bindings, context) as NamedType;
          }
        }
      }
      node.type = type;
      break;
    }
    case "type declaration": {
      // Recursively resolve the type. Also resolves other named types
      // via the "type reference" case above.
      context.withCurrentFunctionOrType(node, () => {
        context.withGenericBindings(() => {
          if (node.type.kind == "named type") {
            // Set up generic bindings for each generic type name with type AnyType.
            const seenGenericTypeNames = new Set<string>();
            for (const genericTypeName of node.genericTypeNames) {
              if (seenGenericTypeNames.has(genericTypeName.value)) {
                throw new LittleFootError(genericTypeName.location, `Duplicate generic type name '${genericTypeName.value}'.`);
              }
              seenGenericTypeNames.add(genericTypeName.value);
              context.addGenericBinding(genericTypeName.value, node.type.getGenericType(genericTypeName.value), genericTypeName.location);
            }

            // Resolve the types in the type specifier.
            const type = types.get(node.name.value)! as NamedType;
            type.type = ResolvingTypeMarker;
            checkNodeTypes(node.typeNode, context);
            if (node.typeNode.type == UnknownType) {
              throw new LittleFootError(node.name.location, `Internal compiler error: named type '${node.name.value}' should have a type set.`);
            }
            node.type.type = node.typeNode.type;

            // Check if all generic types have been used in the type specifier.
            let genericTypes = new Set<string>(node.genericTypeNames.map((type) => type.value));
            traverseType(node.typeNode.type, (type) => {
              if (type.kind == "named type") {
                genericTypes.delete(type.name);
              }
              return true;
            });
            if (genericTypes.size > 0) {
              let missingTypes = [];
              for (const missingType of genericTypes.values()) {
                missingTypes.push(missingType);
              }
              throw new LittleFootError(node.typeNode.location, `Not all generic types used in type specifier: ${missingTypes.join(", ")}.`);
            }
          }
        });
      });

      break;
    }
    case "name and type": {
      checkNodeTypes(node.typeNode, context);
      node.type = node.typeNode.type;
      break;
    }
    case "import":
      // Compile or fetch the already compiled module.
      const oldErrors = [...context.compilerContext.errors];
      context.compilerContext.errors.length = 0;
      const module = compileModule(node.path.value, context.compilerContext);
      const moduleHasErrors = context.compilerContext.errors.length > 0;
      context.compilerContext.errors.push(...oldErrors);
      if (moduleHasErrors) {
        throw new LittleFootError(node.path.location, `Can not import module ${node.path.value} because it has errors.`);
      }

      if (node.importedNames.length == 0) {
        importModule(module, context.module, context);
      } else {
        // Otherwise, import only named things and optionally alias them within
        // this module.
        const seenNames = new Map<string, ImportedNameNode>();
        for (const importedName of node.importedNames) {
          checkNodeTypes(importedName, context);
          if (seenNames.has(importedName.name.value)) {
            throw new LittleFootError(
              importedName.location,
              `Duplicate import ${importedName.name.value}, already specified previously in this import statement.`
            );
          }

          const alias = importedName.alias ? importedName.alias.value : importedName.name.value;
          if (module.types.lookup.has(importedName.name.value)) {
            const type = module.types.lookup.get(importedName.name.value)!;
            if (type.kind == "named type") {
              if (type.location.source.path == module.source.path) {
                context.module.types.add(alias, type);
                if (type.constructorFunction) {
                  context.module.functions.add(alias, type.constructorFunction);
                }
              }
            }
          }
          if (module.functions.has(importedName.name.value)) {
            const funcs = module.functions.get(importedName.name.value)!;
            for (const func of funcs) {
              if (func.location.source.path == module.source.path && func.exported) {
                context.module.functions.add(alias, func);
              }
            }
          }
          if (module.variables.has(importedName.name.value)) {
            const variable = module.variables.get(importedName.name.value)!;
            context.module.variables.set(alias, variable);
            context.addToScope(alias, variable);
          }
        }
      }
      node.type = NothingType;
      break;
    case "imported name": {
      node.type = NothingType;
      break; // no-op, handled in "import" case above
    }
    case "function declaration": {
      context.withCurrentFunctionOrType(node, () => {
        if (!node.returnType && node.genericTypeNames.length > 0) {
          throw new LittleFootError(node.name.location, "Generic functions must have an explicit return type.");
        }
        checkFunctionDeclarationNode(node, context, true);
      });
      break;
    }
    case "variable declaration": {
      checkNodeTypes(node.initializer, context);
      if (node.typeNode) {
        checkNodeTypes(node.typeNode, context);
        node.type = node.typeNode.type;
        const assignable = isAssignableTo(node.initializer, node.type, context);
        node.initializer = assignable.from;
        if (!assignable.isAssignable) {
          // RECOVER: the type of the variable is given, so it doesn't matter that the
          // initializer expression has an error.
          context.compilerContext.errors.push(
            new LittleFootError(node.initializer.location, `Can not assign type '${node.initializer.type.signature}' to '${node.type.signature}'.`)
          );
        }
        node.type = node.typeNode.type;
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
      context.addToScope(node.name.value, node);
      break;
    }
    case "if": {
      checkNodeTypes(node.condition, context);
      if (!(node.condition.type == BooleanType || isGeneric(node.condition.type))) {
        throw new LittleFootError(node.condition.location, `'if' condition must be a boolean but has type '${node.condition.type.signature}'.`);
      }

      // Gather the "is" operators and check if they are negated.
      const isOperators: {
        isNegated: boolean;
        isOperator: IsOperatorNode;
        originalType: Type;
        narrowedType: Type | null;
        negatedType: Type | null;
      }[] = [];

      // Try/finally, as we need to reset the variable types so we can do proper recovery on the
      // statement level.
      let negated = false;
      traverseAst(node.condition, node, (node) => {
        if (node.kind == "unary operator" && node.operator.value == "not") negated = !negated;
        if (node.kind == "is operator") {
          isOperators.push({ isNegated: negated, isOperator: node, originalType: node.leftExpression.type, narrowedType: null, negatedType: null });
        }
        return true;
      });
      try {
        // Temporarily set the types of each variable found in the "is" operators
        // to the type specified in the operator.
        for (const operator of isOperators) {
          if (operator.isOperator.leftExpression.kind != "variable access") {
            throw new LittleFootError(operator.isOperator.leftExpression.location, "Must be a variable.");
          }
          const variable = context.getFromScope((operator.isOperator.leftExpression as VariableAccessNode).name.value)!;
          const variableType = rawType((operator.originalType = variable.type));
          // If the variable is generic, we assume it can be a union
          if (!(variableType.kind == "union" || isGeneric(variableType))) {
            throw new LittleFootError(operator.isOperator.leftExpression.location, "Variable type must be a union.");
          }
          operator.narrowedType = operator.isOperator.typeNode.type;
          if (!typeIsAssignableTo(operator.isOperator.typeNode.type, variable.type)) {
            throw new LittleFootError(
              variable.location,
              `Variable '${variable.name.value}' has type '${variable.type.signature}' and can never be type '${operator.narrowedType.signature}'.`
            );
          }

          if (variableType.kind == "union") {
            // If the variable is generic, we just use it for the negated type set as well, as it's an any type.
            const newTypes = isGeneric(variableType)
              ? [operator.originalType]
              : variableType.types.filter((type) => !typeIsAssignableTo(type, operator.narrowedType!));
            if (newTypes.length == 0 && (operator.isNegated || node.falseBlock.length > 0)) {
              throw new LittleFootError(
                operator.isOperator.location,
                `Negation of 'is' operator results in empty type for '${variable.name.value}' in the ${operator.isNegated ? "true" : "false"} branch.`
              ); // TODO better message
            }
            operator.negatedType = newTypes.length == 1 ? newTypes[0] : new UnionType(newTypes);
          }

          variable.type = operator.isNegated && operator.negatedType ? operator.negatedType : operator.narrowedType;
        }

        context.withScope(() => {
          checkBlock(node.trueBlock, context);
        });

        // Reset the variable types narrowed down in "is" operators
        for (const operator of isOperators) {
          const variable = context.getFromScope((operator.isOperator.leftExpression as VariableAccessNode).name.value)!;
          variable.type = operator.originalType;
        }

        context.withScope(() => {
          checkBlock(node.elseIfs, context);
        });

        // Set the negated types for the else block
        for (const operator of isOperators) {
          const variable = context.getFromScope((operator.isOperator.leftExpression as VariableAccessNode).name.value)!;
          variable.type = operator.isNegated && operator.negatedType ? operator.narrowedType! : operator.negatedType!;
        }

        context.withScope(() => {
          checkBlock(node.falseBlock, context);
        });
      } finally {
        // Reset the variable types
        for (const operator of isOperators) {
          const variable = context.getFromScope((operator.isOperator.leftExpression as VariableAccessNode).name.value)!;
          variable.type = operator.originalType;
        }
      }

      node.type = NothingType;
      break;
    }
    case "while":
      checkNodeTypes(node.condition, context);
      if (node.condition.type != BooleanType) {
        throw new LittleFootError(node.condition.location, `'while' condition must be a boolean but has type '${node.condition.type.signature}'.`);
      }

      context.withLoop(node, () => {
        context.withScope(() => {
          checkBlock(node.block, context);
        });
      });

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

      context.withLoop(node, () => {
        context.withScope(() => {
          context.addToScope(node.loopVariable.name.value, node.loopVariable);
          checkBlock(node.block, context);
        });
      });

      node.type = NothingType;
      break;
    case "for": {
      checkNodeTypes(node.from, context);
      checkNodeTypes(node.to, context);
      if (node.step) checkNodeTypes(node.step, context);

      if (!isNumericType(node.from.type)) {
        throw new LittleFootError(node.from.location, `'from' must be a numeric type, but has type '${node.from.type.signature}'.`);
      }

      if (!isNumericType(node.to.type)) {
        throw new LittleFootError(node.to.location, `'from' must be a numeric type, but has type '${node.to.type.signature}'.`);
      }

      if (node.step && !isNumericType(node.step.type)) {
        throw new LittleFootError(node.step.location, `'from' must be a numeric type, but has type '${node.step.type.signature}'.`);
      }

      let widestType: Type;
      if (node.loopVariable.typeSpecifier) {
        checkNodeTypes(node.loopVariable.typeSpecifier, context);
        widestType = node.loopVariable.type = node.loopVariable.typeSpecifier.type;
        if (node.step && !isNumericType(node.step.type)) {
          throw new LittleFootError(
            node.step.location,
            `Type of loop variable ${node.loopVariable.name} must be a numeric type, but has type '${node.loopVariable.type.signature}'.`
          );
        }
      } else {
        widestType = node.loopVariable.type = findWidestNumericType([node.from.type, node.to.type, node.step ? node.step.type : node.to.type]);
      }

      let assignable = isAssignableTo(node.from, widestType, context);
      node.from = assignable.from;
      if (!assignable.isAssignable) {
        throw new LittleFootError(node.from.location, `'from' must be a '${widestType.signature}', but has type '${node.from.type.signature}'.`);
      }

      assignable = isAssignableTo(node.to, widestType, context);
      node.to = assignable.from;
      if (!assignable.isAssignable) {
        throw new LittleFootError(node.to.location, `'to' must be a '${widestType.signature}', but has type '${node.to.type.signature}'.`);
      }

      if (node.step) {
        assignable = isAssignableTo(node.step, widestType, context);
        node.step = assignable.from;
        if (!assignable.isAssignable) {
          throw new LittleFootError(node.step.location, `'step' must be '${widestType.signature}', but has type '${node.step.type.signature}'.`);
        }
      }

      context.withLoop(node, () => {
        context.withScope(() => {
          context.addToScope(node.loopVariable.name.value, node.loopVariable);
          checkBlock(node.block, context);
        });
      });

      node.type = NothingType;
      break;
    }
    case "do":
      checkNodeTypes(node.condition, context);
      if (node.condition.type != BooleanType) {
        throw new LittleFootError(node.condition.location, `'do' condition must be a boolean but has type '${node.condition.type.signature}'.`);
      }

      context.withLoop(node, () => {
        context.withScope(() => {
          checkBlock(node.block, context);
        });
      });

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
          `Ternary operator ? condition must be a boolean but has type '${node.condition.type.signature}'.`
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
            const symbol = context.getFromScope(node.leftExpression.name.value);
            // This should never happen, as the variable is looked up during checkNodeTypes(node.leftExpression)
            // above via the "variable access" ast node. Stil...
            if (!symbol) {
              throw new LittleFootError(node.leftExpression.name.location, `Can not find variable '${node.leftExpression.name.value}'.`);
            }
            if (symbol.kind == "loop variable") {
              throw new LittleFootError(
                node.leftExpression.name.location,
                `Can not assign a new value to loop variable '${node.leftExpression.name.value}'`
              );
            }
            if (symbol.kind == "variable declaration" && symbol.constant == true) {
              throw new LittleFootError(
                node.leftExpression.name.location,
                `Can not assign a new value to constant '${node.leftExpression.name.value}'`
              );
            }
            const assignable = isAssignableTo(node.rightExpression, node.leftExpression.type, context);
            node.rightExpression = assignable.from;
            if (!assignable.isAssignable) {
              throw new LittleFootError(
                node.rightExpression.location,
                `Can not assign type '${node.rightExpression.type.signature}' to type '${node.leftExpression.type.signature}'`
              );
            }
          } else if (node.leftExpression.kind == "member access") {
            const assignable = isAssignableTo(node.rightExpression, node.leftExpression.type, context);
            node.rightExpression = assignable.from;
            if (!assignable.isAssignable) {
              throw new LittleFootError(
                node.rightExpression.location,
                `Can not assign type '${node.rightExpression.type.signature}' to type '${node.leftExpression.type.signature}'`
              );
            }
          } else if (node.leftExpression.kind == "map or list access") {
            // FIXME check if there's anoperator [][T](list: [T], index, element: T): T; or
            // operator [][T](map: {T}, key: string, element: T): T; we can
            // use for the assignment.
            if (node.leftExpression.target.type.kind == "list") {
              const assignable = isAssignableTo(node.rightExpression, node.leftExpression.target.type.elementType, context);
              node.rightExpression = assignable.from;
              if (!assignable.isAssignable) {
                `Can not assign type '${node.rightExpression.type.signature}' to an array with '${node.leftExpression.target.type.elementType.signature}'`;
              }
            } else if (node.leftExpression.target.type.kind == "map") {
              const assignable = isAssignableTo(node.rightExpression, node.leftExpression.target.type.valueType, context);
              node.rightExpression = assignable.from;
              if (!assignable.isAssignable) {
                `Can not assign type '${node.rightExpression.type.signature}' to an array with '${node.leftExpression.target.type.valueType.signature}'`;
              }
            } else {
              throw new LittleFootError(
                node.leftExpression.target.location,
                `Can not use [] operator with type '${node.leftExpression.target.type}'.`
              );
            }
          } else {
            throw new LittleFootError(node.leftExpression.location, "Left side of assignment must be a variable, record field, list, or map.");
          }
          node.type = node.leftExpression.type;
          break;
        case "==":
        case "!=": {
          // FIXME think about equality, overload operator?
          // Primitive values are checked by value
          // All other types are checked by reference
          // For structural equality, users have to implement their own
          // functions.
          const leftType = node.leftExpression.type;
          const rightType = node.rightExpression.type;
          if (!(isGeneric(leftType) || isGeneric(rightType) || isEqual(leftType, rightType))) {
            throw new LittleFootError(
              node.location,
              `Operands of '${node.operator.value}' operator must have the same type, but are '${leftType.signature}' and '${rightType.signature}'.`
            );
          }
          node.type = BooleanType;
          break;
        }
        case "or":
        case "and":
        case "xor":
        case "<":
        case "<=":
        case ">":
        case ">=":
        case "+":
        case "-":
        case "/":
        case "*":
        case "%": {
          const leftType = node.leftExpression.type;
          const rightType = node.rightExpression.type;
          if (isGeneric(leftType) || isGeneric(rightType)) {
            node.type = AnyType;
          } else {
            let closestFunc = getClosestFunction(context, node.operator.value, [node.leftExpression, node.rightExpression]);
            if (!closestFunc) {
              throw new LittleFootError(
                node.location,
                `Operator '${node.operator.value}' undefined for left operand '${leftType.signature}' and right operand '${rightType.signature}'.`
              );
            }
            if (closestFunc.length > 1) {
              // If the operands are numeric types, try again with the widest possible type
              if (isNumericType(leftType) && isNumericType(rightType)) {
                const widestType = findWidestNumericType([leftType, rightType]);
                const left = coerceNumericTypes(node.leftExpression, widestType);
                const right = coerceNumericTypes(node.rightExpression, widestType);
                closestFunc = getClosestFunction(context, node.operator.value, [left, right]);
                if (!closestFunc) {
                  throw new LittleFootError(
                    node.location,
                    `Operator '${node.operator.value}' undefined for left operand '${leftType.signature}' and right operand '${rightType.signature}'.`
                  );
                } else {
                  if (closestFunc.length > 1) {
                    throw new LittleFootError(
                      node.location,
                      `Found more than one implementation for operator '${node.operator.value}' for left operand '${leftType.signature}' and right operand '${rightType.signature}'.`,
                      reportCandidatesFunctions(closestFunc, false)
                    );
                  } else {
                    node.leftExpression = left;
                    node.rightExpression = right;
                  }
                }
              } else {
                throw new LittleFootError(
                  node.location,
                  `Found more than one implementation for operator '${node.operator.value}' for left operand '${leftType.signature}' and right operand '${rightType.signature}'.`,
                  reportCandidatesFunctions(closestFunc, false)
                );
              }
            }
            node.type = closestFunc[0].type.returnType;
          }
          break;
        }
        default:
          throw new LittleFootError(node.operator.location, `Unknown operator ${node.operator.value}`);
      }
      break;
    case "unary operator":
      checkNodeTypes(node.expression, context);
      switch (node.operator.value) {
        case "not":
          if (!(node.expression.type == BooleanType || isGeneric(node.expression.type))) {
            throw new LittleFootError(
              node.expression.location,
              `Operand of 'not' operator must be a boolean, but has type '${node.expression.type.signature}'`
            );
          }
          node.type = BooleanType;
          break;
        case "+":
        case "-":
          if (!(isNumericType(node.expression.type) || isGeneric(node.expression.type))) {
            throw new LittleFootError(
              node.expression.location,
              `Operand of '${node.operator.value}' operator must be a number, but has type '${node.expression.type.signature}'`
            );
          }
          node.type = node.expression.type;
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
      // FIXME would an uninstantiated generic type be useful for is?
      if (node.typeNode.type.kind == "named type" && !node.typeNode.type.isInstantiated) {
        throw new LittleFootError(node.typeNode.location, `Must specify generic type arguments when using a generic type with 'is' operator.`);
      }
      node.type = BooleanType;
      break;
    case "as operator":
      checkNodeTypes(node.leftExpression, context);
      checkNodeTypes(node.typeNode, context);
      // FIXME would an uninstantiated generic type be useful for as?
      if (node.typeNode.type.kind == "named type" && !node.typeNode.type.isInstantiated) {
        throw new LittleFootError(node.typeNode.location, `Must specify generic type arguments when using a generic type with 'as' operator.`);
      }
      const assignable = isAssignableTo(node.leftExpression, node.typeNode.type, context);
      node.leftExpression = assignable.from;
      if (!assignable.isAssignable) {
        if (isNumericType(node.leftExpression.type) && isNumericType(node.typeNode.type)) {
          throw new LittleFootError(
            node.leftExpression.location,
            `Can not convert numeric type '${node.leftExpression.type.signature}' to numeric type '${node.typeNode.type.signature}, loss of numeric precision.`,
            `Use function '${rawType(node.typeNode.type).signature}(value: ${rawType(node.typeNode.type).signature}): ${
              rawType(node.typeNode.type).signature
            }' to perform an explicit conversion.`
          );
        } else {
          throw new LittleFootError(
            node.leftExpression.location,
            `Can not interpret type '${node.leftExpression.type.signature}' as type '${node.typeNode.type.signature}'`
          );
        }
      }
      node.type = node.typeNode.type;
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
      node.type = checkFunctionLiteralNode(node, context);
      break;
    case "variable access":
      const symbol = context.getFromScope(node.name.value);
      if (!symbol) {
        // User might try to access the variable before its defined. This can happen like this
        //
        // foo()
        // var a = 0
        // func foo() print(a) end
        //
        // We check if there are module level variables with that name and if so, tell the user
        // that they mustn't call foo() before a was declared, either directly, or indirectly through
        // another call.
        //
        const variable = context.module.variables.get(node.name.value);
        if (!variable) {
          throw new LittleFootError(node.name.location, `Can not find variable '${node.name.value}'.`);
        }

        throw new LittleFootError(node.name.location, `Access to module-level variable '${node.name.value}' before it is initialized.`);
      }
      node.type = symbol.type;
      break;
    case "member access":
      checkNodeTypes(node.object, context);
      const type = rawType(node.object.type);
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
          throw new LittleFootError(node.member.location, `Field '${node.member.value}' does not exist on type '${node.object.type.signature}'.`);
        }
      } else {
        if (!context.isInGenericFunctionOrTypeDeclaration()) {
          throw new LittleFootError(node.member.location, `Field '${node.member.value}' does not exist on type '${node.object.type.signature}'.`);
        } else {
          // If we are type checking a generic function, set the type to any type so we can continue
          // type checking.
          node.type = AnyType;
        }
      }
      break;
    case "map or list access":
      // FIXME check if there's an operator [][T](list: [T], index: number): T; or
      // operator [][T](map: {T}, key: string): T;
      checkNodeTypes(node.keyOrIndex, context);
      checkNodeTypes(node.target, context);
      if (node.target.type.kind == "list") {
        if (node.keyOrIndex.type != NumberType) {
          throw new LittleFootError(node.keyOrIndex.location, `Index into list must be a number, but has type '${node.keyOrIndex.type.signature}'.`);
        }
        node.type = node.target.type.elementType;
      } else if (node.target.type.kind == "map") {
        if (node.keyOrIndex.type != StringType) {
          throw new LittleFootError(node.keyOrIndex.location, `Index into map must be a string, but has type '${node.keyOrIndex.type.signature}'.`);
        }
        node.type = node.target.type.valueType;
      } else {
        throw new LittleFootError(
          node.target.location,
          `The '[]' operator can only be used with lists or maps, but was used with type ${node.target.type.signature}.`
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
        const symbol = context.getFromScope(node.target.name.value);
        if (symbol) {
          if (rawType(symbol.type).kind != "function") {
            throw new LittleFootError(node.target.name.location, `'${node.target.name.value}' is not a function.`);
          }
          const functionType = rawType(symbol.type) as FunctionType;
          if (functionType.parameters.length != node.args.length) {
            throw new LittleFootError(node.location, `Expected ${functionType.parameters.length} arguments, got ${node.args.length}.`);
          }
          for (let i = 0; i < node.args.length; i++) {
            let arg = node.args[i];
            const param = functionType.parameters[i];
            const assignable = isAssignableTo(arg, param.type, context);
            arg = node.args[i] = assignable.from;
            if (!assignable.isAssignable) {
              throw new LittleFootError(arg.location, `Expected type ${param.type.signature}, got ${arg.type.signature}`);
            }
          }
          checkNodeTypes(node.target, context);
          node.type = functionType.returnType;
        } else {
          // Otherwise, lookup the best fitting function for the given args,
          // inferClosestFunction will also call checkFunctionNode in case the
          // function hasn't been checked yet.
          const closestFunc = inferClosestFunction(node.location, node.target, node.target.name.value, node.args, context);
          node.target.type = closestFunc;
          node.type = closestFunc.type.returnType;
        }
      } else {
        // Function call on function returned through map or list element access.
        checkNodeTypes(node.target, context);
        if (node.target.type.kind != "function") {
          throw new LittleFootError(node.target.location, `Target of function call is not a function, but got type '${node.target.type.signature}'.`);
        }
        const functionType = node.target.type;
        if (functionType.parameters.length != node.args.length) {
          throw new LittleFootError(node.location, `Expected ${functionType.parameters.length} arguments, but got ${node.args.length}.`);
        }
        for (let i = 0; i < node.args.length; i++) {
          let arg = node.args[i];
          const param = functionType.parameters[i];
          const assignable = isAssignableTo(arg, param.type, context);
          arg = node.args[i] = assignable.from;
          if (!assignable.isAssignable) {
            throw new LittleFootError(arg.location, `Expected type ${param.type.signature}, but got ${arg.type.signature}`);
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
          // Possibly a member holding a reference to a function
          if (field.type.kind != "function") {
            throw new LittleFootError(node.target.member.location, `'${node.target.member.value}' is not a function.`);
          }
          const functionType = field.type;
          if (functionType.parameters.length != node.args.length) {
            throw new LittleFootError(node.location, `Expected ${functionType.parameters.length} arguments, got ${node.args.length}.`);
          }
          for (let i = 0; i < node.args.length; i++) {
            let arg = node.args[i];
            const param = functionType.parameters[i];
            const assignable = isAssignableTo(arg, param.type, context);
            arg = node.args[i] = assignable.from;
            if (!assignable.isAssignable) {
              throw new LittleFootError(arg.location, `Expected type ${param.type.signature}, got ${arg.type.signature}`);
            }
          }
          node.target.type = functionType;
          node.type = functionType.returnType;
          return;
        }
        // Fall through if no member function was found
      }
      // Otherwise, lookup the best fitting function for the given args, including the "object"
      // as the first argument. inferClosestFunction will also call checkFunctionNode in case the
      // function has no return type assigned yet.
      const args = [node.target.object, ...node.args];
      const closestFunc = inferClosestFunction(node.location, node.target, node.target.member.value, args, context);
      node.target.type = closestFunc;
      node.type = closestFunc.type.returnType;
      break;
    case "incomplete expression":
      checkNodeTypes(node.expression, context);
      node.type = node.expression.type;
      throw node.error;
    case "numeric widening":
      checkNodeTypes(node.expression, context);
      break;
    case "union expansion":
      checkNodeTypes(node.expression, context);
      break;
    default:
      assertNever(node);
  }
}

function reportCandidatesFunctions(functions: NamedFunctionType[] | undefined, showArgCount = true) {
  return `${
    functions && functions.length > 0
      ? "Candidates: \n" +
        functions
          .map(
            (func) =>
              indent(1) +
              func.signatureWithParameterNames() +
              " (" +
              func.location.toString() +
              ")" +
              (showArgCount ? func.type.parameters.length + ", argument" + (func.type.parameters.length > 1 ? "s" : "") : "")
          )
          .join("\n")
      : ""
  }`;
}

function reportFunctionNotFound(location: SourceLocation, name: string, args: ExpressionNode[], context: TypeCheckerContext) {
  const functions = context.module.functions;
  const candidates = functions.get(name);

  // Function with that name doesn't exist
  if (!candidates) {
    return new LittleFootError(location, `A function with the name '${name}' does not exist.`);
  }

  // No function with that number of arguments exist, list candidates
  const sameArgCountCandidates = candidates.filter((func) => func.type.parameters.length == args.length);
  if (sameArgCountCandidates.length == 0) {
    return new LittleFootError(
      location,
      `Can not find function '${name}(${args.map((arg) => arg.type.signature).join(",")})' with ${args.length} arguments.`,
      reportCandidatesFunctions(functions.get(name))
    );
  }

  // Functions with that name exist, but argument types don't match parameter types, report problematic arguments
  const originalArgTypes: Type[] = [];
  for (const arg of args) {
    originalArgTypes.push(arg.type);
  }
  let supplementary = `Candidates:`;
  for (const func of sameArgCountCandidates) {
    supplementary += "\n\n" + indent(1) + func.signatureWithParameterNames() + " (" + func.location.toString() + ")\n";
    for (let i = 0; i < args.length; i++) {
      const param = func.type.parameters[i].type;
      const arg = args[i];
      if (!isEqual(arg.type, param)) {
        arg.type = arg.type.copy();
        try {
          if (!isAssignableTo(arg, param, context).isAssignable) {
            supplementary += `${indent(2)}Parameter '${func.type.parameters[i].name}': expected type '${param.signature}', but got type '${
              arg.type.signature
            }'.`;
          }
        } catch (e) {
          let exceptionMessage = "";
          if (e instanceof LittleFootError) {
            exceptionMessage = e
              .toString()
              .split("\n")
              .map((line) => indent(3) + line)
              .join("\n");
          } else {
            exceptionMessage = ((e as any).message as string)
              .split("\n")
              .map((line) => indent(3) + line)
              .join("\n");
          }

          if (arg.kind == "list literal" && arg.type.kind == "list" && arg.elements.length == 0) {
            supplementary += `${indent(2)}Parameter '${func.type.parameters[i].name}': can not infer type of empty list\n${exceptionMessage}`;
          } else {
            if (arg.kind == "map literal" && arg.type.kind == "map" && arg.values.length == 0) {
              supplementary += `${indent(2)}Parameter '${func.type.parameters[i].name}': can not infer type of empty map\n${exceptionMessage}`;
            } else {
              supplementary += `${indent(2)}Parameter '${func.type.parameters[i].name}':\n${exceptionMessage}`;
            }
          }
        }
      }
    }
    for (let i = 0; i < args.length; i++) {
      args[i].type = originalArgTypes[i];
    }
  }
  let errorMessage = `Can not find function '${name}' with matching arguments '(${args.map((arg) => arg.type.signature).join(",")})'.`;
  return new LittleFootError(location, errorMessage, supplementary);
}

function checkFunctionDeclarationNode(node: FunctionNode, context: TypeCheckerContext, checkCode: boolean) {
  // If this function calls itself, it needs to have a return type.
  if (node.isBeingChecked && node.returnType == null) {
    throw new LittleFootError(node.name.location, "Functions that are called recursively, either directly or indirectly, must have a return type.");
  }

  node.isBeingChecked = true;
  context.withScope(() => {
    context.withGenericBindings(() => {
      if (!checkCode) {
        // If we aren't checking the code, then this means we are validating the signature of the
        // function declaration and construct and assign the NamedFunctionType to it.

        // Generate dummy generic bindings.
        node.genericTypeNames.forEach((typeName) => context.addGenericBinding(typeName.value, newGenericAnyType(typeName), typeName.location));

        // Assign types to the function's parameters and return type
        for (const parameter of node.parameters) {
          checkNodeTypes(parameter, context);
          context.addToScope(parameter.name.value, parameter);
        }
        if (node.returnType) checkNodeTypes(node.returnType, context);

        // Check if all generic types are being used by parameters and/or return type
        let genericTypes = new Set<string>(node.genericTypeNames.map((type) => type.value));
        for (const parameter of node.parameters) {
          traverseType(parameter.type, (type) => {
            if (type.kind == "named type") {
              genericTypes.delete(type.name);
            }
            return true;
          });
        }
        if (genericTypes.size > 0) {
          let missingTypes = [];
          for (const missingType of genericTypes.values()) {
            missingTypes.push(missingType);
          }
          throw new LittleFootError(node.name.location, `Not all generic types used in function parameter list: ${missingTypes.join(", ")}.`);
        }

        // Create the named function type and assign it to the node.
        const functionType = new FunctionType(
          node.parameters.map((parameter) => new NameAndType(parameter.name.value, parameter.type)),
          node.returnType ? node.returnType.type : UnknownType
        );
        const namedFunction = new NamedFunctionType(
          node.name.value,
          node.genericTypeNames.map((name) => new NameAndType(name.value, context.getGenericBinding(name.value)!)),
          node.genericTypeNames.length == 0,
          functionType,
          node,
          node.exported,
          node.external,
          node.location
        );
        node.type = namedFunction;
      } else {
        // Otheriwse, we check the code block and check or infer the return type
        // of the function.
        if (node.type.kind != "named function") {
          throw new LittleFootError(node.name.location, "Internal error: expected function to be typed.");
        }
        node.type.genericTypes.forEach((genericType, index) =>
          context.addGenericBinding(genericType.name, genericType.type, node.genericTypeNames[index].location)
        );

        // Check parameters and return type, which may also have concrete generic bindings now.
        for (const parameter of node.parameters) {
          checkNodeTypes(parameter, context);
          context.addToScope(parameter.name.value, parameter);
        }
        if (node.returnType) checkNodeTypes(node.returnType, context);

        try {
          checkBlock(node.code, context);
        } catch (e) {
          if (e instanceof LittleFootError && e.message.startsWith("Access to module-level variable")) {
          } else {
            throw e;
          }
        }

        const returnType = checkOrInferFunctionReturnType(node, context);
        if (!node.returnType) {
          node.type.type.setReturnType(returnType);
          node.type.updateSignature();
        }
      }
    });
  });
  node.isBeingChecked = false;
  return node.type as NamedFunctionType;
}

function checkFunctionLiteralNode(node: FunctionLiteralNode, context: TypeCheckerContext) {
  context.withModuleScope(() => {
    // Check parameters and return type.
    for (const parameter of node.parameters) {
      checkNodeTypes(parameter, context);
      context.addToScope(parameter.name.value, parameter);
    }
    if (node.returnType) checkNodeTypes(node.returnType, context);

    // Check the code block
    checkBlock(node.code, context);

    // Check or infer return type
    const returnType = checkOrInferFunctionReturnType(node, context);
    const functionType = new FunctionType(
      node.parameters.map((parameter) => new NameAndType(parameter.name.value, parameter.type)),
      returnType
    );
    node.type = functionType;
  });
  return node.type;
}

function checkOrInferFunctionReturnType(node: FunctionNode | FunctionLiteralNode, context: TypeCheckerContext) {
  // FIXME need to ensure all exit paths actually return the return type. Need a CFG.
  if (node.returnType) {
    // If a return type was given, check that the returned expressions are assignable to it.
    const returnType = node.returnType.type;
    for (const statement of node.code) {
      traverseAst(statement, node, (node) => {
        if (node.kind == "return") {
          if (node.expression) {
            node.expression = isAssignableTo(node.expression, returnType, context).from;
            node.type = node.expression.type;
          }
          if (!typeIsAssignableTo(node.type, returnType)) {
            throw new LittleFootError(
              node.location,
              `Can not return a value of type '${node.type.signature}' from a function with return type '${returnType.signature}'.`
            );
          }
        }
        return true;
      });
    }
    return returnType;
  } else {
    // Otherwise gather the types and infere the return type.
    const returns: ReturnNode[] = [];
    for (const statement of node.code) {
      traverseAst(statement, node, (node) => {
        if (node.kind == "return") {
          returns.push(node);
        }
        return true;
      });
    }
    const returnTypes = returns.map((ret) => ret.type);
    if (returnTypes.length == 0) returnTypes.push(NothingType);
    const returnType = returnTypes.length == 1 ? returnTypes[0] : unify(returnTypes[0], new UnionType(returnTypes));
    return returnType;
  }
}

function checkBlock(block: StatementNode[], context: TypeCheckerContext) {
  for (const statement of block) {
    try {
      checkNodeTypes(statement, context);
    } catch (e) {
      const error: LittleFootError =
        e instanceof LittleFootError
          ? e
          : new LittleFootError(
              new SourceLocation(statement.location.source, 0, 1),
              "Internal error: " + (e as any).message + "\n" + (e as any).stack
            );
      if (statement.kind == "return" || statement.kind == "variable declaration" || context.isInGenericFunctionOrTypeDeclaration(true)) {
        throw error;
      } else {
        context.compilerContext.errors.push(error);
      }
    }
  }
}

export function importModule(fromModule: Module, toModule: Module, context: TypeCheckerContext) {
  // Import all exported things defined in the module if no names are given.
  // We do not export anything transiently.
  fromModule.types.lookup.forEach((type, name) => {
    if (type.kind == "named type") {
      if (type.location.source.path == fromModule.source.path && type.exported) {
        toModule.types.add(name, type);
        if (type.constructorFunction) {
          toModule.functions.add(name, type.constructorFunction);
        }
      }
    }
  });
  fromModule.functions.lookup.forEach((funcs, name) => {
    for (const func of funcs) {
      if (func.location.source.path == fromModule.source.path && func.exported) {
        toModule.functions.add(name, func);
      }
    }
  });
  for (const name of fromModule.variables.keys()) {
    const variable = fromModule.variables.get(name)!;
    if (variable.location.source.path == fromModule.source.path && variable.exported) {
      toModule.variables.set(name, variable);
      context.addToScope(name, variable);
    }
  }
}

/**
 * FIXME update the function docs for all functions below involved in
 * type inference, union expansion, and coercion.
 *
 * Inferres types for `from` if necessary based on `to`, then checks
 * if `from` is assignable to `to`.
 *
 * Type inference for `from` is necessary if `from` has empty list or map
 * literals, so the runtime can instantiate the correct types for the empty
 * lists and map literals. The types of these literals are inferred from `to`.
 *
 * If `to` includes unions, and `from` is a literal, then corresponding parts of
 * `from`'s type must be expanded to unions.
 */
function isAssignableTo(from: ExpressionNode, to: Type, context: TypeCheckerContext): { isAssignable: boolean; from: ExpressionNode } {
  inferTypesOfEmptyListAndMapLiterals(from, to, context);
  from = expandLiteralValueTypesToUnions(from, to, context);
  from = coerceNumericTypes(from, to);
  const isAssignable = typeIsAssignableTo(from.type, to);
  // Final pass for unions. If from is assignable and itself not a union
  // and to is a union, box from.
  if (isAssignable && rawType(from.type).kind != "union" && rawType(to).kind == "union") {
    from = new UnionExpansionNode(from, to);
  }
  return { isAssignable, from };
}

// Finds empty lists and maps in literals and infers their type based on the
// `to` type.
function inferTypesOfEmptyListAndMapLiterals(from: AstNode, to: Type, context: TypeCheckerContext): boolean {
  const toType = rawType(to);

  // The from type has an empty list or map literal in it. We need to infer
  // its type if possible. The from type must be a (nested) list, map, or record at
  // this point, and to must have a corresponding type.
  if (hasEmptyListOrMap(from.type)) {
    if (from.kind == "list literal" && from.type.kind == "list") {
      // If the from list literal is not nested and empty, it has type
      // UnknownType. Assign the to type.
      if (toType.kind == "list") {
        if (from.elements.length == 0 && from.type.elementType.signature == UnknownType.signature) {
          from.type = to;
        } else {
          // Otherwise, the from list literal has a nested value with an
          // unknown type. Recursively check and resolve the unknown types
          // of empty list or map literal values.
          for (const element of from.elements) {
            if (!inferTypesOfEmptyListAndMapLiterals(element, toType.elementType, context)) return false;
          }
          from.type.setElementType(nodeListToType(from.elements));
        }
        return true;
      }

      // `to` can also be a union.
      if (toType.kind == "union") {
        const listTypes = toType.types.filter((type) => type.kind == "list");
        if (listTypes.length == 0) {
          // If there's no list type in the union, the empty list type can not be inferred.
          throw new LittleFootError(from.location, `Can not infer type of empty list literal from target type ${to.signature}`);
        } else if (listTypes.length == 1) {
          // If the union contains 1 list type, try to assign that as the empty list type
          return inferTypesOfEmptyListAndMapLiterals(from, listTypes[0], context);
        } else {
          // If there are > 1 list types, the type is undecideable
          throw new LittleFootError(
            from.location,
            `Can not infer type of empty list literal from target type ${to.signature}.`,
            `Candidates:\n${listTypes.map((type) => indent(1) + type.signature).join("\n")}`
          );
        }
      }

      // `to` can be a generic type parameter
      if (toType == AnyType) {
        throw new LittleFootError(from.location, `Can not infer empty list literal type from generic parameter ${to.signature}.`);
      }

      return true;
    } else if (from.kind == "map literal" && from.type.kind == "map") {
      if (toType.kind == "map") {
        // If the from list literal is not nested and empty, it has type
        // UnknownType. Assign the to type.
        if (from.type.kind == "map" && from.type.valueType.signature == UnknownType.signature) {
          from.type = to;
        } else {
          // Otherwise, the from map literal has a nested value with an
          // unknown type. Recursively check and resolve the unknown types
          // of the values.
          for (const value of from.values) {
            if (!inferTypesOfEmptyListAndMapLiterals(value, toType.valueType, context)) return false;
          }

          // Update the map literal's type.
          from.type.setValueType(nodeListToType(from.values));
        }
        return true;
      }

      if (toType.kind == "union") {
        // `to` can also be a union.
        const mapTypes = toType.types.filter((type) => type.kind == "map");
        if (mapTypes.length == 0) {
          // If there's no list type in the union, the empty list type can not be inferred.
          throw new LittleFootError(from.location, `Can not infer type of empty map literal from target type ${to.signature}`);
        } else if (mapTypes.length == 1) {
          // If the union contains 1 list type, try to assign that as the empty list type
          return inferTypesOfEmptyListAndMapLiterals(from, mapTypes[0], context);
        } else {
          // If there are > 1 list types, the type is undecideable
          throw new LittleFootError(
            from.location,
            `Can not infer type of empty map literal from target type ${to.signature}.`,
            `Candidates:\n${mapTypes.map((type) => indent(1) + type.signature).join("\n")}`
          );
        }
      }

      // `to` can be a generic type parameter
      if (toType == AnyType) {
        throw new LittleFootError(from.location, `Can not infer empty list literal type from generic parameter ${to.signature}.`);
      }
      return true;
    }

    if (from.kind == "record literal" && toType.kind == "record") {
      // If the from record has less fields than the to record
      // we can stop infering types. The from record can not be
      // assigned to the to record.
      if (from.fieldValues.length < toType.fields.length) return false;

      // Otherwise, fix up empty lists and maps in the record's field
      // which will also resolve their respective unknown types.
      for (let i = 0; i < from.fieldValues.length; i++) {
        const fieldName = from.fieldNames[i];
        const fieldValue = from.fieldValues[i];
        let found = false;
        for (const toField of toType.fields) {
          if (fieldName.value !== toField.name) continue;
          if (inferTypesOfEmptyListAndMapLiterals(fieldValue, toField.type, context)) {
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
  }

  return true;
}

/**
 * Expands values in list, map, and record literals to unions if the
 * to type is a union. Needed to ensure the memory layouts of
 * all involved types match.
 */
function expandLiteralValueTypesToUnions(from: ExpressionNode, to: Type, context: TypeCheckerContext): ExpressionNode {
  const toType = rawType(to);

  if (hasUnion(to)) {
    // If from is a primitive type, "box" it.
    if (
      typeIsAssignableTo(from.type, toType) &&
      toType.kind == "union" &&
      from.type.kind == "primitive" &&
      (from.kind == "string literal" || from.kind == "boolean literal" || from.kind == "number literal" || from.kind == "nothing literal")
    ) {
      return new UnionExpansionNode(from, to);
    }

    // If from is a list literal and to is a list type expand
    // from's element type to a union if necessary.
    if (from.kind == "list literal" && from.type.kind == "list" && toType.kind == "list") {
      const toElementType = rawType(toType.elementType);
      // If to's element type is a union, unify from's element type
      // with the union.
      if (toElementType.kind == "union") {
        if (typeIsAssignableTo(from.type.elementType, toElementType)) {
          for (let i = 0; i < from.elements.length; i++) {
            from.elements[i] = new UnionExpansionNode(from.elements[i], toType.elementType);
          }
          from.type.setElementType(toType.elementType);
        } else {
          // For each element in the literal, find the best
          // candidate type in the union, then expand to it.
          // Once all candidates have been identified, unify
          // them into a single union and assign that
          // as the element type of from.
          let foundAll = true;
          for (let i = 0; i < from.elements.length; i++) {
            const element = from.elements[i];
            const oldType = element.type.copy();
            let candidates: Type[] = [];
            for (const unionType of toElementType.types) {
              // If we have a perfect type match, select it
              if (isEqual(element.type, unionType)) {
                candidates = [unionType];
                break;
              }
              // Otherwise, check if from is assignable to to and if so
              // add it to the list of candidates.
              if (isAssignableTo(element, unionType, context).isAssignable) {
                candidates.push(unionType);
              }
              element.type = oldType.copy();
            }
            element.type = oldType;
            if (candidates.length == 1) {
              // expand the list, maps, or record internal types to unions if necessary
              from.elements[i] = isAssignableTo(element, candidates[0], context).from;
            } else {
              if (candidates.length > 1) {
                throw new LittleFootError(
                  from.location,
                  `Tried to expand type ${from.type.signature} to a type in union ${toType.elementType.signature}, but multiple candidates were found.`,
                  `Candidates:\n` + candidates.map((type) => indent(1) + type.signature).join("\n")
                );
              } else {
                // No candidate found, this will result in an error
                // When typeIsAssignable is called later.
                foundAll = false;
              }
            }
          }

          if (foundAll) {
            // If we found candidates in the union for all elements
            // we expand them to the full union type and set the
            // element type of from accordingly.
            for (let i = 0; i < from.elements.length; i++) {
              from.elements[i] = new UnionExpansionNode(from.elements[i], toType.elementType);
            }
            from.type.setElementType(toType.elementType);
          } else {
            // Othwerise, this is only a partial match, which will
            // trigger an error in the remainder of the type checking
            // of isAssignableTo(). We do a best effort union.
            let newElementType = new UnionType([]);
            for (const element of from.elements) {
              newElementType = unify(element.type, newElementType);
            }
            for (let i = 0; i < from.elements.length; i++) {
              from.elements[i] = new UnionExpansionNode(from.elements[i], newElementType);
            }
            from.type.setElementType(newElementType);
          }
        }
        return from;
      }

      // Otherwise, if to's element type is a list, expand from's
      // value types recursively.
      if (toElementType.kind == "list" || toElementType.kind == "map" || toElementType.kind == "record") {
        if (from.elements.length > 0) {
          for (let i = 0; i < from.elements.length; i++) {
            from.elements[i] = expandLiteralValueTypesToUnions(from.elements[i], toType.elementType, context);
          }
          from.type.setElementType(nodeListToType(from.elements));
        }
      }
      return from;
    }

    // If from is a map literal and to is a map type expand
    // from's value type to a union if necessary.
    if (from.kind == "map literal" && from.type.kind == "map" && toType.kind == "map") {
      const toValueType = rawType(toType.valueType);
      if (toValueType.kind == "union") {
        // If to's value type is a union, unify from's value type
        // with the union.
        if (typeIsAssignableTo(from.type.valueType, toValueType)) {
          for (let i = 0; i < from.values.length; i++) {
            from.values[i] = new UnionExpansionNode(from.values[i], toType.valueType);
          }
          from.type.setValueType(toType.valueType);
        } else {
          // For each element in the literal, find the best
          // candidate type in the union, then expand to it.
          // Once all candidates have been identified, unify
          // them into a single union and assign that
          // as the element type of from.
          let foundAll = true;
          for (let i = 0; i < from.values.length; i++) {
            const element = from.values[i];
            const oldType = element.type.copy();
            let candidates: Type[] = [];
            for (const unionType of toValueType.types) {
              // If we have a perfect type match, select it
              if (isEqual(element.type, unionType)) {
                candidates = [unionType];
                break;
              }
              // Otherwise, check if from is assignable to to and if so
              // add it to the list of candidates.
              if (isAssignableTo(element, unionType, context).isAssignable) {
                candidates.push(unionType);
              }
              element.type = oldType.copy();
            }
            element.type = oldType;
            if (candidates.length == 1) {
              // expand the list, maps, or record internal types to unions if necessary
              from.values[i] = isAssignableTo(element, candidates[0], context).from;
            } else {
              if (candidates.length > 1) {
                throw new LittleFootError(
                  from.location,
                  `Tried to expand type ${from.type.signature} to a type in union ${toValueType.signature}, but multiple candidates were found.`,
                  `Candidates:\n` + candidates.map((type) => type.signature).join("\n")
                );
              } else {
                // No candidate found, this will result in an error
                // When typeIsAssignable is called later.
                foundAll = false;
              }
            }
          }

          if (foundAll) {
            // If we found candidates in the union for all values
            // we expand them to the full union type and set the
            // values type of from accordingly.
            for (let i = 0; i < from.values.length; i++) {
              from.values[i] = new UnionExpansionNode(from.values[i], toType.valueType);
            }
            from.type.setValueType(toType.valueType);
          } else {
            // Othwerise, this is only a partial match, which will
            // trigger an error in the remainder of the type checking
            // of isAssignableTo(). We do a best effort union.
            let newElementType = new UnionType([]);
            for (const element of from.values) {
              newElementType = unify(element.type, newElementType);
            }
            for (let i = 0; i < from.values.length; i++) {
              from.values[i] = new UnionExpansionNode(from.values[i], newElementType);
            }
            from.type.setValueType(newElementType);
          }
        }
        return from;
      }

      // Otherwise, if to's value type is a map, expand from's
      // value types recursively.
      if (toValueType.kind == "list" || toValueType.kind == "map" || toValueType.kind == "record") {
        if (from.values.length > 0) {
          for (let i = 0; i < from.values.length; i++) {
            from.values[i] = expandLiteralValueTypesToUnions(from.values[i], toType.valueType, context);
          }
          from.type.setValueType(nodeListToType(from.values));
        }
      }
      return from;
    }

    // If from is a record literal and to is a record type expand
    // from's field types to a union if necessary.
    if (from.kind == "record literal" && from.type.kind == "record" && toType.kind == "record") {
      // If from has less fields than to, the two record types
      // can't be compatible so don't do anything.
      if (from.fieldValues.length < toType.fields.length) return from;

      for (let i = 0; i < from.fieldValues.length; i++) {
        const fieldName = from.fieldNames[i];
        const fieldValue = from.fieldValues[i];
        let found = false;
        for (const toField of toType.fields) {
          if (fieldName.value !== toField.name) continue;
          from.fieldValues[i] = expandLiteralValueTypesToUnions(fieldValue, toField.type, context);
          found = true;
          break;
        }
        if (!found) return from;
      }
      for (let i = 0; i < from.fieldValues.length; i++) {
        from.type.fields[i].type = from.fieldValues[i].type;
      }
      from.type.updateSignature();
      return from;
    }

    // if to is a union and from is a list, map, or record literal:
    // 1. Find the best candidate in the union type list that is equal to from's type, or to which
    //    from is assiganble to.
    // 2. Expand from's parts to the found candidate.
    //
    // If more than one candidate is found, then from needs to be type annotated to help pick the
    // best candidate. Report this as an error.
    if (toType.kind == "union" && (from.kind == "list literal" || from.kind == "map literal" || from.kind == "record literal")) {
      const oldType = from.type.copy();
      let candidates: Type[] = [];
      for (const unionType of toType.types) {
        // If we have a perfect type match, select it
        if (isEqual(from.type, unionType)) {
          candidates = [unionType];
          break;
        }
        // Otherwise, check if from is assignable to to and if so
        // add it to the list of candidates.
        if (isAssignableTo(from, unionType, context).isAssignable) {
          candidates.push(unionType);
        }
        from.type = oldType.copy();
      }
      from.type = oldType;
      if (candidates.length == 1) {
        // expand the list, maps, or record internal types to unions if necessary
        const assignable = isAssignableTo(from, candidates[0], context);
        from = new UnionExpansionNode(assignable.from, to);
      } else {
        if (candidates.length > 1) {
          throw new LittleFootError(
            from.location,
            `Tried to expand type ${from.type.signature} to a type in union ${to.signature}, but multiple candidates were found.`,
            `Candidates:\n` + candidates.map((type) => indent(1) + type.signature).join("\n")
          );
        }
      }
    }
  }
  return from;
}

const numericTypes = new Set([Int8Type, Int16Type, Int32Type, Float32Type, Float64Type]);
const canWidenFrom = new Map<PrimitiveType, Set<PrimitiveType>>();
canWidenFrom.set(Int8Type, new Set([Int16Type, Int32Type, Float32Type, Float64Type]));
canWidenFrom.set(Int16Type, new Set([Int32Type, Float32Type, Float64Type]));
canWidenFrom.set(Int32Type, new Set([Float32Type, Float64Type]));
canWidenFrom.set(Float32Type, new Set([Float64Type]));

function isNumericType(type: Type) {
  const rType = rawType(type);
  if (rType.kind != "primitive") return false;
  return numericTypes.has(rType);
}

function findWidestNumericType(types: Type[]): PrimitiveType {
  let widestType = UnknownType;
  for (const type of types) {
    if (!isNumericType(type)) continue;
    if (type.kind == "primitive" && type.id > widestType.id) {
      widestType = type;
    }
  }
  if (widestType == UnknownType) return Float64Type;
  return widestType;
}

function getNarrowestNumericType(num: number, isHex = false) {
  let isFloat = !(num % 1 === 0);

  if (!isFloat && num >= -128 && num <= (isHex ? 255 : 127)) return Int8Type;
  if (!isFloat && num >= -32768 && num <= (isHex ? 65535 : 32767)) return Int16Type;
  if (!isFloat && num >= -2147483648 && num <= (isHex ? 4294967295 : 2147483647)) return Int32Type;
  if (num >= -3.4028234663852886e38 && num <= 3.4028234663852886e38) return Float32Type;
  return Float64Type;
}

function coerceNumericTypes(from: ExpressionNode, to: Type): ExpressionNode {
  let fromType = rawType(from.type);
  const toType = rawType(to);

  // Coerce numeric values in list literals
  if (from.kind == "list literal" && fromType.kind == "list" && toType.kind == "list") {
    const toElementType = rawType(toType.elementType);
    const fromElementType = rawType(fromType.elementType);

    if (isNumericType(toElementType) && isNumericType(fromElementType)) {
      // If both element types are numeric, coerce the elements of from
      // and set its element type accordingly.
      for (let i = 0; i < from.elements.length; i++) {
        from.elements[i] = coerceNumericTypes(from.elements[i], toElementType);
      }
      fromType.setElementType(toElementType);
    } else {
      // Otherwise, if the element types are lists, recurse down the type hierarchy
      if (toElementType.kind == "list" && fromElementType.kind == "list") {
        for (let i = 0; i < from.elements.length; i++) {
          coerceNumericTypes(from.elements[i], toElementType);
        }
        fromType.setElementType(toElementType);
      }
    }
    return from;
  }

  // Coerce numeric values in map literals
  if (from.kind == "map literal" && fromType.kind == "map" && toType.kind == "map") {
    const toValueType = rawType(toType.valueType);
    const fromValueType = rawType(fromType.valueType);

    if (isNumericType(toValueType) && isNumericType(fromValueType)) {
      // If both element types are numeric, coerce the elements of from
      // and set its element type accordingly.
      for (let i = 0; i < from.values.length; i++) {
        from.values[i] = coerceNumericTypes(from.values[i], toValueType);
      }
      fromType.setValueType(toValueType);
    } else {
      // Otherwise, if the element types are lists, recurse down the type hierarchy
      if (toValueType.kind == "list" && fromValueType.kind == "list") {
        for (let i = 0; i < from.values.length; i++) {
          coerceNumericTypes(from.values[i], toValueType);
        }
        fromType.setValueType(toValueType);
      }
    }
    return from;
  }

  // Coerce numeric values in record literals
  if (from.kind == "record literal" && fromType.kind == "record" && toType.kind == "record") {
    // If from has less fields than to, the two record types
    // can't be compatible so don't do anything.
    if (from.fieldValues.length < toType.fields.length) return from;

    for (let i = 0; i < from.fieldValues.length; i++) {
      const fieldName = from.fieldNames[i];
      const fieldValue = from.fieldValues[i];
      let found = false;
      for (const toField of toType.fields) {
        if (fieldName.value !== toField.name) continue;
        from.fieldValues[i] = coerceNumericTypes(fieldValue, toField.type);
        found = true;
        break;
      }
      if (!found) return from;
    }
    for (let i = 0; i < from.fieldValues.length; i++) {
      fromType.fields[i].type = from.fieldValues[i].type;
    }
    fromType.updateSignature();
    return from;
  }

  // If from is a number literal, narrow its type based on its value if possible.
  if (from.kind == "number literal") {
    if (!from.token.value.includes(".")) {
      from.type = fromType = getNarrowestNumericType(from.token.numericValue, from.token.value.startsWith("0x") || from.token.value.startsWith("0b"));
    }
  }

  // When to is a union, and from is a numeric type, try to find
  // a fitting type in the union and coerce to it.
  if (toType.kind == "union" && isNumericType(fromType) && fromType.kind == "primitive") {
    let candidates: Type[] = [];
    for (const unionType of toType.types) {
      // If it's a perfect match, select it.
      if (isEqual(fromType, unionType)) {
        candidates.push(unionType);
        continue;
      }
      // Otherwise, check if from can be widened to to and if so
      // add it to the list of candidates.
      if (unionType.kind == "primitive" && canWidenFrom.get(fromType)?.has(unionType)) {
        candidates.push(unionType);
      }
    }
    if (candidates.length == 1) {
      // Create a widening node
      from = new NumericWideningNode(from, candidates[0]);
    } else {
      if (candidates.length > 1) {
        throw new LittleFootError(
          from.location,
          `Tried to widen numeric type ${from.type.signature} to a type in union ${to.signature}, but multiple candidates were found.`,
          `Candidates:\n` + candidates.map((type) => indent(1) + type.signature).join("\n")
        );
      }
    }
    return from;
  }

  // We've reached the leaves in the types. From here on, both must be
  // numeric types. If that's not the case, we bail. Otherwise, we
  // coerce the from node if necessary and possible.
  if (fromType.kind != "primitive") return from;
  if (toType.kind != "primitive") return from;
  if (!isNumericType(fromType) || !isNumericType(toType)) return from;

  // If they are the same type, we do nothing
  if (fromType == toType) return from;

  // Otherwise check if from can be widened to and add a widening node
  // The caller must replace the original node with the widening node.
  if (canWidenFrom.get(fromType)?.has(toType)) {
    from = new NumericWideningNode(from, toType);
  }
  return from;
}

function inferClosestFunction(location: SourceLocation, target: AstNode, name: string, args: ExpressionNode[], context: TypeCheckerContext) {
  // Lookup the best fitting functions for the given args. This will also
  // call checkFunctionNode in case the function has no return type set yet.
  let closestFunc = getClosestFunction(context, name, args);
  if (!closestFunc) {
    throw reportFunctionNotFound(location, name, args, context);
  }

  // If there's more than one best fit, error. User has to narrow argument list.
  // Don't error in case we are type checking a generic function. The arguments
  // could result in a single best fit when instantiated.
  if (closestFunc.length > 1 && !context.isInGenericFunctionOrTypeDeclaration()) {
    throw new LittleFootError(
      location,
      `More than one function called '${name}' matches the arguments.`,
      `Candidates: \n${closestFunc.map((func) => indent(1) + func.signature + " (" + func.location.toString() + ")").join("\n")}`
    );
  }

  // Prevent cycles in generic function instantiations by checking the returned function
  // is the one we currently check.
  if (closestFunc[0].ast == context.getCurrentFunctionOrType()) {
    return closestFunc[0];
  }

  // If the closest function is a generic function, we have a few scenarios to handle:
  // 1. The call is part of checking a non-generic function declaration. In this case
  //    we infer its types based on the arguments and instantiate it.
  // 2. The call is part of checking a generic function declaration. In this case, we do not infer
  //    the generic types of the closest function, nor do we instantiate it.
  // 3. The call is part of checking a generic function invocation, which means it has generic type
  //    bindings set. In this case we infer its types based on the arguments and instantiate it.
  if (!closestFunc[0].isInstantiated && closestFunc[0].genericTypes.length > 0) {
    let genericBindings = new Map<string, Type>();
    const concreteFunctionParameters: NameAndType[] = [];
    for (let i = 0; i < args.length; i++) {
      const name = closestFunc[0].type.parameters[i].name;
      const type = args[i].type;
      concreteFunctionParameters.push(new NameAndType(name, type));
    }
    const concretefunctionType = new FunctionType(concreteFunctionParameters, UnknownType);
    genericBindings = inferGenericTypes(closestFunc[0].ast, closestFunc[0], target, concretefunctionType);

    try {
      let funcType = closestFunc[0];
      context.withCurrentFunctionOrType(funcType.ast, () => {
        funcType = instantiateGenericType(target, funcType, genericBindings, context) as NamedFunctionType;
      });
      context.withCurrentFunctionOrType(funcType.ast, () => {
        checkFunctionDeclarationNode(funcType.ast, context, true);
      });
      return funcType;
    } catch (e) {
      const cause: LittleFootError =
        e instanceof LittleFootError
          ? e
          : new LittleFootError(new SourceLocation(location.source, 0, 1), "Internal error: " + (e as any).message + "\n" + (e as any).stack);
      throw new LittleFootError(location, `Can not instantiate generic function ${closestFunc[0].signatureWithParameterNames()}`, "", cause);
    }
  }
  return closestFunc[0];
}

function getClosestFunction(context: TypeCheckerContext, name: string, args: ExpressionNode[]) {
  const funcs = context.module.functions.get(name);
  if (!funcs) return null;

  // Score each function. scoreFunction will return a score of Number.MAX_VALUE
  // if the function isn't compatible with the arguments.
  const scoredFunctions: { score: number; func: NamedFunctionType }[] = [];
  for (const func of funcs) {
    if (func.type.returnType == UnknownType) {
      checkFunctionDeclarationNode(func.ast, context, true);
    }
    const score = scoreFunction(func, args, context);
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
  let candidates = scoredFunctions.filter((scoredFunc) => scoredFunc.score == bestScore).map((scoredFunc) => scoredFunc.func);

  // If there is a single instantiated best candidate, use that and discard
  // the rest. This removes generic functions in favor of instantiated ones.
  const instantiatedCandidates = candidates.filter((func) => func.isInstantiated);
  if (instantiatedCandidates.length == 1) {
    candidates = instantiatedCandidates;
  }

  // If we found a single candidate, type empty lists and expand literal value types to union types
  // in the arguments. Don't do this for non-instantiated generic functions or else generic type
  // inference will fail.
  if (candidates.length == 1 && (candidates[0].genericTypes.length == 0 || !isGeneric(candidates[0]))) {
    const func = candidates[0];
    for (let i = 0; i < args.length; i++) {
      const param = func.type.parameters[i].type;
      const arg = args[i];
      args[i] = isAssignableTo(arg, param, context).from;
    }
  }
  return candidates;
}

function scoreFunction(func: NamedFunctionType, args: ExpressionNode[], context: TypeCheckerContext) {
  if (func.type.parameters.length != args.length) return Number.MAX_VALUE;

  let match = true;
  let score = 0;
  const originalArgTypes: Type[] = [];
  for (const arg of args) {
    originalArgTypes.push(arg.type);
  }
  try {
    // For each argument, calculate a score:
    // 1. If the types match, add 3 to the score
    // 2. If the argument type is assignable to the
    //    parameter type, add 1 to the score if its a generic
    //    type, and 2 if it is a non-generic type.
    //
    // This way functions with more direct type matches
    // between arguments and parameters will score higher
    // overall.
    for (let i = 0; i < args.length; i++) {
      const param = func.type.parameters[i].type;
      const arg = args[i];
      if (isEqual(arg.type, param)) {
        score += 3;
      } else {
        // Need to copy the argument node, as isAssignableTo
        // may modify its type hierarchy.
        arg.type = arg.type.copy();
        if (isAssignableTo(arg, param, context).isAssignable) {
          // Penalize generic parameters, which have type AnyType.
          score += isGeneric(param) ? 1 : 2;
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

function inferGenericTypes(genericNode: AstNode, genericType: NamedType | NamedFunctionType, concreteNode: AstNode, concreteType: Type) {
  // Infer the generic type parameters from the concrete type(s). This is a simultanious
  // traversal of the generic type and conrecte type tree. If a generic type is encountered in the
  // generic tree, its concrete type counter part is recorded as a binding.
  const infer = (node: AstNode, genericType: Type, concreteType: Type, genericTypeBindings: Map<string, Type[]>) => {
    // We've arrived at the generic type, set the concrete type as its binding.
    if (genericType.kind == "named type" && genericType.type == AnyType) {
      let bindings = genericTypeBindings.get(genericType.name);
      if (!bindings) {
        bindings = [];
        genericTypeBindings.set(genericType.name, bindings);
      }
      bindings.push(concreteType);
      return;
    }

    // Special case for types like type leaf[T] = T, when the concrete type is not generic. In this case T = concrete type.
    if (
      genericType.kind == "named type" &&
      genericType.genericTypes.length == 1 &&
      genericType.type == genericType.genericTypes[0].type &&
      !isGeneric(concreteType)
    ) {
      let bindings = genericTypeBindings.get(genericType.genericTypes[0].name);
      if (!bindings) {
        bindings = [];
        genericTypeBindings.set(genericType.genericTypes[0].name, bindings);
      }
      bindings.push(concreteType);
      return;
    }

    switch (genericType.kind) {
      case "function":
        if (concreteType.kind == "function") {
          for (let i = 0; i < genericType.parameters.length; i++) {
            infer(node, genericType.parameters[i].type, concreteType.parameters[i].type, genericTypeBindings);
          }
          // Only infer by return type, if the return type isn't unknown. It can be unknown if
          // the function is called free standingly, e.g. add(1, 2)
          if (concreteType.returnType != UnknownType) {
            infer(node, genericType.returnType, concreteType.returnType, genericTypeBindings);
          }
        } else {
          throw new LittleFootError(
            node.location,
            `Internal error: generic type ${genericType.signature} != concrete type ${concreteType.signature}.`
          );
        }
        break;
      case "primitive":
        // leaf in type which can not be a generic type
        break;
      case "list":
        if (concreteType.kind == "list") {
          infer(node, genericType.elementType, concreteType.elementType, genericTypeBindings);
        } else {
          throw new LittleFootError(
            node.location,
            `Internal error: generic type ${genericType.signature} != concrete type ${concreteType.signature}.`
          );
        }
        break;
      case "map":
        if (concreteType.kind == "map") {
          infer(node, genericType.valueType, concreteType.valueType, genericTypeBindings);
        } else {
          throw new LittleFootError(
            node.location,
            `Internal error: generic type ${genericType.signature} != concrete type ${concreteType.signature}.`
          );
        }
        break;
      case "record":
        if (concreteType.kind == "record") {
          for (let i = 0; i < genericType.fields.length; i++) {
            infer(node, genericType.fields[i].type, concreteType.fields[i].type, genericTypeBindings);
          }
        } else {
          throw new LittleFootError(
            node.location,
            `Internal error: generic type ${genericType.signature} != concrete type ${concreteType.signature}.`
          );
        }
        break;
      case "union":
        if (concreteType.kind == "union") {
          for (let i = 0; i < genericType.types.length; i++) {
            // If the concrete type was infered via union expansion, it might contain
            // generic types. We do not want to use those for inference.
            if (!isGeneric(concreteType.types[i])) {
              infer(node, genericType.types[i], concreteType.types[i], genericTypeBindings);
            }
          }
        } else {
          // If the concrete type is not a union, we must infer based on the union type
          // the concrete type is assignable to. This can happen if the concrete type
          // stems from a value assigned to a union, e.g.
          // type t[L] = <value: L | nothing>
          // var a = 1;
          // t(a)
          // a has type number, but is assignable to t(value: L | nothing).
          for (let i = 0; i < genericType.types.length; i++) {
            if (typeIsAssignableTo(concreteType, genericType.types[i])) {
              infer(node, genericType.types[i], concreteType, genericTypeBindings);
              break;
            }
          }
        }
        break;
      case "named type":
        // Extract bindings from concrete generic type if available.
        if (concreteType.kind == "named type") {
          // The raw types must match at least
          if (rawType(concreteType).kind != rawType(genericType).kind) {
            // generic type might be a union, let's see if we can infer something
            // from that.
            if (rawType(genericType).kind == "union") {
              infer(node, rawType(genericType), concreteType, genericTypeBindings);
              break;
            } else {
              throw new LittleFootError(
                node.location,
                `Generic type ${genericType.signature} is not the same kind as concrete type ${concreteType.signature}.`
              );
            }
          }

          if (genericType.genericTypes.length != concreteType.genericTypes.length) {
            throw new LittleFootError(
              node.location,
              `Internal error: number of bindings (${genericType.genericTypes.length}) for generic type ${genericType.signature} != number of bindings (${concreteType.genericTypes.length}) for concrete type ${concreteType.signature}.`
            );
          }
          genericType.genericTypes.forEach((genericBinding) => {
            if (genericBinding.type.kind == "named type" && genericBinding.type.type == AnyType) {
              const concreteBinding = concreteType.getGenericType(genericBinding.name);
              if (!concreteBinding) {
                throw new LittleFootError(
                  node.location,
                  `Internal error: can not find concrete binding for generic type ${genericBinding.name} in named type.`
                );
              }
              let bindings = genericTypeBindings.get(genericBinding.type.name);
              if (!bindings) {
                bindings = [];
                genericTypeBindings.set(genericBinding.type.name, bindings);
              }
              bindings.push(concreteBinding);
            }
          });
        } else {
          // Otherwise, check if we can extract something from the raw type.
          if (rawType(genericType).kind == concreteType.kind || rawType(genericType).kind == "union") {
            infer(node, rawType(genericType), concreteType, genericTypeBindings);
          } else {
            throw new LittleFootError(
              node.location,
              `Internal error: generic type ${genericType.signature} != concrete type ${concreteType.signature}.`
            );
          }
        }
        break;
      case "named function":
        throw new LittleFootError(concreteNode.location, `Internal error: found named function type in concrete type.`);
      default:
        assertNever(genericType);
    }
  };
  const genericTypeBindings = new Map<string, Type[]>();
  infer(genericNode, genericType.type, concreteType, genericTypeBindings);

  // For each generic type, merge its found bindings and check
  // if there's only 1 binding left after the merge. Set that
  // binding as the final type of the generic type.
  const finalBindings = new Map<string, Type>();
  for (const genericTypeName of genericTypeBindings.keys()) {
    const bindings = genericTypeBindings.get(genericTypeName)!;
    const seenSignatures = new Set<string>();
    const unifiedBindings: Type[] = [];
    for (const binding of bindings) {
      if (!seenSignatures.has(binding.signature)) {
        seenSignatures.add(binding.signature);
        unifiedBindings.push(binding);
      }
    }
    if (unifiedBindings.length > 1) {
      throw new LittleFootError(
        concreteNode.location,
        `Found multiple types for generic type parameter ${genericTypeName} of generic ${
          genericType.kind == "named function" ? "function" : "type"
        } ${genericType.signature}. Ensure empty lists and maps have a concrete type, e.g. [:number] instead of [], Candidates:\n${unifiedBindings
          .map((type) => indent(1) + type.signature)
          .join("\n")}`
      );
    }
    finalBindings.set(genericTypeName, unifiedBindings[0]);
  }
  return finalBindings;
}

function instantiateGenericType(
  node: AstNode,
  genericType: NamedType | NamedFunctionType,
  genericTypeBindings: Map<string, Type>,
  context: TypeCheckerContext
) {
  // Construct a type with the bindings
  let genericBoundType = genericType.copy();
  const replacedNamedTypes = new Set<Type>();
  const replace = (type: Type, bindings: Map<string, Type>): Type => {
    if (type.kind == "named type" && type.type == AnyType) {
      const binding = bindings.get(type.name);
      if (!binding) {
        throw new LittleFootError(genericType.location, `Internal error: can't resolve generic binding ${type.name}`);
      }
      replacedNamedTypes.add(type);
      return binding;
    }

    switch (type.kind) {
      case "function":
        for (let i = 0; i < type.parameters.length; i++) {
          const parameter = type.parameters[i];
          parameter.type = replace(parameter.type, bindings);
        }
        type.returnType = replace(type.returnType, bindings);
        break;
      case "primitive":
        // leaf in type which can not be a generic type
        break;
      case "list":
        type.elementType = replace(type.elementType, bindings);
        break;
      case "map":
        type.valueType = replace(type.valueType, bindings);
        break;
      case "record":
        for (const field of type.fields) {
          field.type = replace(field.type, bindings);
        }
        break;
      case "union":
        for (let i = 0; i < type.types.length; i++) {
          type.types[i] = replace(type.types[i], bindings);
        }
        break;
      case "named type":
        // handle recursive named types
        if (!replacedNamedTypes.has(type)) {
          replacedNamedTypes.add(type);
          type.type = replace(type.type, bindings);
          for (const genericType of type.genericTypes) {
            genericType.type = replace(genericType.type, bindings);
          }
          type.isInstantiated = true;
        }
        break;
      case "named function":
        // handle recursive named types
        if (!replacedNamedTypes.has(type)) {
          replacedNamedTypes.add(type);
          type.type = replace(type.type, bindings) as FunctionType;
          for (const genericType of type.genericTypes) {
            genericType.type = replace(genericType.type, bindings);
          }
          type.isInstantiated = true;
        }
        break;
      default:
        assertNever(type);
    }
    return type;
  };
  genericBoundType = replace(genericBoundType, genericTypeBindings) as NamedType | NamedFunctionType;
  genericBoundType.updateSignature();

  if (!context.isInGenericFunctionOrTypeDeclaration() && isGeneric(genericBoundType)) {
    throw new LittleFootError(
      node.location,
      `Can not infer all types for generic ${genericType.kind == "named function" ? "function" : "type"} '${
        genericType.signature
      }' from empty list or map literals.`
    );
  }

  if (genericBoundType.kind == "named type") {
    if (context.module.types.has(genericBoundType.signature)) {
      return context.module.types.get(genericBoundType.signature) as NamedType;
    } else {
      // Only add the type if it doesn't have generic types. This happens when
      // evaluating a generic type which includes another generic type, binding
      // the first type's generic types to the second type.
      if (!isGeneric(genericBoundType)) {
        context.module.types.add(genericBoundType.signature, genericBoundType);
      }
      return genericBoundType;
    }
  } else {
    // Copy the AST as well and set generic type bindings
    // FIXME deep copy? All instantiations share the same AST which means
    // the assigned node types are all over the place. Bad for code-gen.
    const ast = genericBoundType.ast as FunctionNode;
    const astCopy = new FunctionNode(
      ast.location,
      ast.name,
      ast.genericTypeNames,
      ast.parameters,
      ast.returnType,
      ast.code,
      ast.exported,
      ast.external
    );
    astCopy.type = genericBoundType;
    genericBoundType.ast = astCopy;

    if (context.module.functions.hasExact(genericBoundType.name, genericBoundType.signature)) {
      return context.module.functions.getExact(genericBoundType.name, genericBoundType.signature)!;
    } else {
      // Only add the function if it doesn't have generic types. This happens when
      // evaluating a generic function which calls another generic function, binding
      // the first function's generic types to the second function.
      if (!isGeneric(genericBoundType)) {
        context.module.functions.add(genericBoundType.name, genericBoundType);
      }
      return genericBoundType;
    }
  }
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

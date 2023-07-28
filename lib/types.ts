import { AstNode, FunctionLiteralNode, FunctionNode, TypeNode } from "./ast";
import { LittleFootError } from "./error";
import { SourceLocation } from "./source";

export abstract class BaseType {
  public abstract kind: string;
  public signature: string = "";

  abstract copy(): Type;

  abstract updateSignature(): void;
}

export class NameAndType {
  constructor(public readonly name: string, public type: Type) {}
}

export class PrimitiveType extends BaseType {
  public readonly kind: "primitive" = "primitive";

  constructor(public readonly name: string) {
    super();
    this.updateSignature();
  }

  copy(): PrimitiveType {
    return this;
  }

  updateSignature(): void {
    this.signature = this.name;
  }
}

export class ListType extends BaseType {
  public readonly kind: "list" = "list";

  constructor(public elementType: Type) {
    super();
    this.updateSignature();
  }

  setElementType(elementType: Type) {
    this.elementType = elementType;
    this.updateSignature();
  }

  copy(): ListType {
    return new ListType(this.elementType.copy());
  }

  updateSignature(): void {
    this.elementType.updateSignature();
    this.signature = "[" + this.elementType.signature + "]";
  }
}

export class MapType extends BaseType {
  public readonly kind: "map" = "map";

  constructor(public valueType: Type) {
    super();
    this.updateSignature();
  }

  setValueType(valueType: Type) {
    this.valueType = valueType;
    this.updateSignature();
  }

  copy(): MapType {
    return new MapType(this.valueType.copy());
  }

  updateSignature(): void {
    this.valueType.updateSignature();
    this.signature = "{" + this.valueType.signature + "}";
  }
}

export class RecordType extends BaseType {
  public readonly kind: "record" = "record";

  constructor(public readonly fields: NameAndType[]) {
    super();
    fields.sort(); // needed for equality and assignability checks
    this.updateSignature();
  }

  copy(): RecordType {
    return new RecordType(this.fields.map((field) => new NameAndType(field.name, field.type.copy())));
  }

  updateSignature() {
    for (const field of this.fields) {
      field.type.updateSignature();
    }
    this.signature =
      "<" +
      this.fields
        .map((field) => field.name + ":" + field.type.signature)
        .sort()
        .join(",") +
      ">";
  }
}

export class FunctionType extends BaseType {
  public readonly kind: "function" = "function";

  constructor(public readonly parameters: NameAndType[], public returnType: Type) {
    super();
    this.updateSignature();
  }

  setReturnType(returnType: Type) {
    this.returnType = returnType;
    this.updateSignature();
  }

  copy(): FunctionType {
    const paramsCopy = this.parameters.map((param) => new NameAndType(param.name, param.type.copy()));
    return new FunctionType(paramsCopy, this.returnType.copy());
  }

  updateSignature(): void {
    for (const parameter of this.parameters) {
      parameter.type.updateSignature();
    }
    this.returnType.updateSignature();
    this.signature = "(" + this.parameters.map((param) => param.type.signature).join(",") + "):" + this.returnType.signature;
  }
}

export class UnionType extends BaseType {
  public readonly kind: "union" = "union";

  constructor(public readonly types: Type[]) {
    super();
    this.updateSignature();
  }

  copy(): UnionType {
    return new UnionType(this.types.map((type) => type.copy()));
  }

  updateSignature(): void {
    for (const type of this.types) {
      type.updateSignature();
    }
    this.signature = this.types
      .map((type) => type.signature)
      .sort()
      .join("|");
  }
}

export class NamedType extends BaseType {
  public readonly kind: "named type" = "named type";
  private updating = false;

  public constructorFunction: NamedFunctionType | null = null;
  constructor(
    public readonly name: string,
    public readonly genericTypeNames: string[],
    public genericTypeBindings: Map<String, Type>,
    public type: Type,
    public typeNode: TypeNode,
    public readonly exported: boolean,
    public readonly location: SourceLocation
  ) {
    super();
    this.updateSignature();
  }

  copy(): NamedType {
    return new NamedType(this.name, this.genericTypeNames, this.genericTypeBindings, this.type.copy(), this.typeNode, this.exported, this.location);
  }

  updateGenericTypeBindings(genericTypeBindings: Map<String, Type>) {
    this.genericTypeBindings = genericTypeBindings;
    this.updateSignature();
  }

  updateSignature(): void {
    if (this.updating) return;
    this.updating = true;
    this.type.updateSignature();
    if (this.genericTypeBindings.size == 0) {
      this.signature = this.name + (this.genericTypeNames.length > 0 ? "[" + this.genericTypeNames.join(",") + "]" : "");
    } else {
      this.signature =
        this.name +
        (this.genericTypeNames.length > 0
          ? "[" + this.genericTypeNames.map((name) => this.genericTypeBindings.get(name)?.signature).join(",") + "]"
          : "");
    }
    this.updating = false;
  }
}

export class NamedFunctionType extends BaseType {
  public readonly kind: "named function" = "named function";
  private updating = false;

  constructor(
    public readonly name: string,
    public readonly genericTypeNames: string[],
    public genericTypeBindings: Map<String, Type>,
    public type: FunctionType,
    public ast: FunctionNode | FunctionLiteralNode,
    public readonly exported: boolean,
    public readonly external: boolean,
    public readonly location: SourceLocation
  ) {
    super();
    this.updateSignature();
  }

  updateReturnType() {
    this.type = this.ast.type as FunctionType;
    this.updateSignature();
  }

  updateGenericTypeBindings(genericTypeBindings: Map<String, Type>) {
    this.genericTypeBindings = genericTypeBindings;
    this.updateSignature();
  }

  copy(): NamedFunctionType {
    return new NamedFunctionType(
      this.name,
      this.genericTypeNames,
      this.genericTypeBindings,
      this.type.copy(),
      this.ast,
      this.exported,
      this.external,
      this.location
    );
  }

  updateSignature(): void {
    if (this.updating) return;
    this.updating = true;
    this.type.updateSignature();
    if (this.genericTypeBindings.size == 0) {
      this.signature = this.name + (this.genericTypeNames.length > 0 ? "[" + this.genericTypeNames.join(",") + "]" : "") + this.type.signature;
    } else {
      this.signature =
        this.name +
        (this.genericTypeNames.length > 0
          ? "[" + this.genericTypeNames.map((name) => this.genericTypeBindings.get(name)?.signature).join(",") + "]"
          : "") +
        this.type.signature;
    }
    this.updating = false;
  }
}

export type Type = PrimitiveType | ListType | MapType | RecordType | FunctionType | UnionType | NamedType | NamedFunctionType;

export const NothingType = new PrimitiveType("nothing");
export const BooleanType = new PrimitiveType("boolean");
export const NumberType = new PrimitiveType("number");
export const StringType = new PrimitiveType("string");
export const UnknownType = new PrimitiveType("$unknown");
export const AnyType = new PrimitiveType("$any"); // Used for generics so we can do some type checking.
export const ResolvingTypeMarker = new PrimitiveType("$resolving");

function assertNever(x: never) {
  throw new Error("Unexpected object: " + x);
}

export function traverseType(type: Type, callback: (type: Type) => boolean, namedTypesStack = new Set<Type>()) {
  if (!callback(type)) return;
  switch (type.kind) {
    case "function":
      for (const parameter of type.parameters) {
        traverseType(parameter.type, callback, namedTypesStack);
      }
      traverseType(type.returnType, callback, namedTypesStack);
      break;
    case "primitive":
      break;
    case "list":
      traverseType(type.elementType, callback, namedTypesStack);
      break;
    case "map":
      traverseType(type.valueType, callback, namedTypesStack);
      break;
    case "record":
      for (const field of type.fields) {
        traverseType(field.type, callback, namedTypesStack);
      }
      break;
    case "union":
      for (const unionType of type.types) {
        traverseType(unionType, callback, namedTypesStack);
      }
      break;
    case "named type":
      if (namedTypesStack.has(type)) break;
      namedTypesStack.add(type);
      traverseType(type.type, callback, namedTypesStack);
      namedTypesStack.delete(type);
      break;
    case "named function":
      traverseType(type.type, callback, namedTypesStack);
      break;
    default:
      assertNever(type);
  }
}

export class Functions {
  public readonly lookup = new Map<string, NamedFunctionType[]>();

  has(name: string) {
    return this.lookup.has(name);
  }

  hasExact(name: string, signature: string) {
    const funcs = this.lookup.get(name);
    if (!funcs) return false;

    for (const func of funcs) {
      if (func.signature == signature) return true;
    }
    return false;
  }

  get(name: string) {
    return this.lookup.get(name);
  }

  getExact(name: string, signature: string) {
    const funcs = this.lookup.get(name);
    if (!funcs) return undefined;

    for (const func of funcs) {
      if (func.signature == signature) return func;
    }
    return undefined;
  }

  add(name: string, func: NamedFunctionType) {
    if (this.hasExact(name, func.signature)) {
      const otherFunc = this.getExact(name, func.signature)! as NamedFunctionType;
      // Adding the exact same function is allowed so
      // module import handling is easier.
      // FIXME check via identity is bad, use location instead
      if (func === otherFunc) {
        return;
      }
      throw new LittleFootError(func.location, `Duplicate function '${name}', first defined in ${otherFunc.location.toString()}.`);
    }

    let funcs = this.lookup.get(name);
    if (!funcs) {
      funcs = [];
      this.lookup.set(name, funcs);
    }
    funcs.push(func);
  }
}

export class Types {
  public readonly lookup = new Map<string, PrimitiveType | NamedType>();

  constructor() {
    this.add(NothingType.name, NothingType);
    this.add(BooleanType.name, BooleanType);
    this.add(NumberType.name, NumberType);
    this.add(StringType.name, StringType);
    this.add(UnknownType.name, UnknownType);
  }

  get(name: string) {
    return this.lookup.get(name);
  }

  has(name: string) {
    return this.lookup.has(name);
  }

  add(name: string, type: PrimitiveType | NamedType) {
    if (this.has(name)) {
      const otherType = this.get(name)!;
      if (otherType.kind == "primitive") {
        if (type.kind != "primitive") {
          throw new LittleFootError(type.location, `Can not use '${name}' as a type name, as a built-in type with that name exists.`);
        } else {
          throw new Error("Tried to add a built-in type twice");
        }
      } else {
        if (type.kind != "primitive") {
          // FIXME check via identity is bad, use location instead
          if (type === otherType) {
            return;
          }
          throw new LittleFootError(type.location, `Duplicate type '${name}', first defined in ${otherType.location.toString()}.`);
        } else {
          throw new Error("Tried to add a built-in type twice");
        }
      }
    } else {
      this.lookup.set(name, type);
    }
  }
}

export function isEqual(from: Type, to: Type) {
  // If both types are named types, then they are only
  // equal if they are the same type by identity. Unless
  // their concrete types are AnyType
  // FIXME check by identity is bad, use location instead
  // This is needed to stop the recursion for types like
  // type node = <children: [node], value: number>
  if (from.kind == "named type" && to.kind == "named type") {
    if (from.type == AnyType && to.type == AnyType) {
      return true;
    } else {
      return from === to;
    }
  }

  // Unpack the type of named types.
  if (from.kind == "named function" || from.kind == "named type") {
    from = from.type;
  }
  if (to.kind == "named function" || to.kind == "named type") {
    to = to.type;
  }

  // Primitive types must match by name exactly.
  if (from.kind == "primitive" && to.kind == "primitive") {
    return from.name == to.name;
  }

  // List types must have equal element types.
  if (from.kind == "list" && to.kind == "list") {
    return isEqual(from.elementType, to.elementType);
  }
  // Map types must have equal value types.
  if (from.kind == "map" && to.kind == "map") {
    return isEqual(from.valueType, to.valueType);
  }

  // Records must have the same number of fields. Each
  // pair of fields must match in both name and type.
  if (from.kind == "record" && to.kind == "record") {
    if (from.fields.length != to.fields.length) return false;
    for (let i = 0; i < from.fields.length; i++) {
      const fromField = from.fields[i];
      let found = false;
      for (const toField of to.fields) {
        if (fromField.name !== toField.name) continue;
        found = true;
        if (!isEqual(fromField.type, toField.type)) return false;
      }
      if (!found) return false;
    }
    return true;
  }

  // Unions must have the same number of types. Each type
  // of a must be found in b.
  //
  // FIXME the current implementation assumes that all types
  // within a union are distinct. However, the following
  // degenerate case can arise (e.g. when mixins are involved):
  //
  // type a = someType | someType | someType
  // type b = someType | anotherType | anotherType
  //
  // These two types are not equal, even though all types
  // of a can be found in b. The current implementation will
  // report these two types as equal.
  if (from.kind == "union" && to.kind == "union") {
    if (from.types.length != to.types.length) return false;
    for (let i = 0; i < from.types.length; i++) {
      const aType = from.types[i];
      let found = false;
      for (let j = 0; j < to.types.length; j++) {
        const bType = to.types[j];
        if (isEqual(aType, bType)) {
          found = true;
          break;
        }
      }
      if (!found) return false;
    }
    return true;
  }

  // Functions must have the same number of parameters. Each
  // parameter pair must have equal types. The return types
  // must also be equal. The parameter names
  // are irrelevant for equality.
  if (from.kind == "function" && to.kind == "function") {
    if (from.parameters.length != to.parameters.length) return false;
    for (let i = 0; i < from.parameters.length; i++) {
      if (!isEqual(from.parameters[i].type, to.parameters[i].type)) return false;
    }
    if (!isEqual(from.returnType, to.returnType)) return false;
    return true;
  }

  // Otherwise, the types are not equal
  return false;
}

// If from is a List and map with element/valueType equal to UnknownType,
// its element/valueType will be set to `to`'s element/valueType
// and it will be reported to be assignable. This allows empty
// list and map literals to be assigned to variables, fields,
// function arguments and so on.
export function isAssignableTo(from: Type, to: Type): boolean {
  // If both types are named types, then they are only
  // equal if they are the same type by identity.
  // FIXME check by identity is bad, use location instead
  // This is needed to stop the recursion for types like
  // type node = <children: [node], value: number>
  if (from.kind == "named type" && to.kind == "named type") {
    return from === to;
  }

  // Unpack the type of named types
  if (from.kind == "named function" || from.kind == "named type") {
    from = from.type;
  }
  if (to.kind == "named function" || to.kind == "named type") {
    to = to.type;
  }

  // If to is the any type, anything is assignable
  if (to == AnyType) {
    return true;
  }

  // This handles primitives and is also an early out for
  // exact type matches. This also handles exact record matches
  // Non-exact matches are handled below.
  if (isEqual(from, to)) return true;

  // If `from` is a union and `to` is not, then `from``
  // can not be assigned
  if (from.kind == "union" && to.kind != "union") {
    return false;
  }

  // If `to` is a union, there are two cases:
  // 1. If `from` is not a union, then it must be assignable
  //    to at least one type in the `to` union. From must be boxed
  //    in this case!
  // 2. If `from` is a union, then all of `from`'s types must be
  //    assignable to at least one type in `to`.
  if (to.kind == "union") {
    if (from.kind != "union") {
      for (const type of to.types) {
        if (isAssignableTo(from, type)) return true;
      }
      return false;
    } else {
      for (const aType of from.types) {
        let found = false;
        for (const bType of to.types) {
          if (isAssignableTo(aType, bType)) {
            found = true;
            break;
          }
        }
        if (!found) return false;
      }
      return true;
    }
  }

  // For lists, if the to element type is a union
  // the from type must be a union as well, so we honor memory layouts.
  // E.g. [number] can not be assigned to [number | string].
  // The from element type must be assignable to the to element type
  if (from.kind == "list" && to.kind == "list") {
    if (to.elementType.kind == "union" && from.elementType.kind != "union") {
      return false;
    }
    return isAssignableTo(from.elementType, to.elementType);
  }

  // For maps, if the to value type is a union then
  // the from value type must be a union as well, so we honor memory layouts.
  // E.g. {"a": number} can not be assigned to {"a": number | string}.
  // The from element type must be assignable to the to element type
  if (from.kind == "map" && to.kind == "map") {
    if (to.valueType.kind == "union" && from.valueType.kind != "union") {
      return false;
    }
    return isAssignableTo(from.valueType, to.valueType);
  }

  // We only get here if isEqual(from, to) was false, which means
  // the two record types don't have the same number or exact
  // types of fields.
  //
  // However, if we can assign all fields of from to a subset of fields
  // of record to, we can assign from to to.
  //
  // If the from and to field match by name, but to is a union and from is not
  // then from is not assignable to to, to honor memory layouts. E.g.
  // <x: number> can not be assigned to <x: number | string>.
  if (from.kind == "record" && to.kind == "record") {
    if (from.fields.length < to.fields.length) return false;
    let matchedFields = 0;
    for (const fromField of from.fields) {
      let found = false;
      for (const toField of to.fields) {
        if (fromField.name !== toField.name) continue;

        // If the to field is a union and the from field is not, the
        // from is not assignable to. This is required to honor memory layouts.
        if (toField.type.kind == "union" && fromField.type.kind != "union") continue;

        // Otherwise, from needs to be assignable to to.
        if (isAssignableTo(fromField.type, toField.type)) {
          matchedFields++;
          if (matchedFields == to.fields.length) return true;
          found = true;
          break;
        }
      }
      if (!found) return false;
    }
    return true;
  }

  // For functions, the number of parameters must match,
  // and the parameter types of `from` must be equal to
  // the parameter types of `. The return type of a must
  // also be assignable to b. The parameter names do not matter.
  if (from.kind == "function" && to.kind == "function") {
    if (from.parameters.length != to.parameters.length) return false;
    for (let i = 0; i < from.parameters.length; i++) {
      if (!isAssignableTo(from.parameters[i].type, to.parameters[i].type)) return false;
    }
    if (!isAssignableTo(from.returnType, to.returnType)) return false;
    return true;
  }

  // Otherwise, the from is not assignable to to
  return false;
}

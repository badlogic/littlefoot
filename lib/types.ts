import { FunctionNode, NumberLiteralNode, StringLiteralNode, TypeNode } from "./ast";
import { LittleFootError } from "./error";
import { SourceLocation } from "./source";

let generateTypeIds = false;
let nextTypeId = 0;

export function setGenerateTypeIds(generate: boolean) {
  generateTypeIds = generate;
}

export function setNextTypeId(id: number) {
  nextTypeId = id;
}

export const seenTypes: Type[] = [];
export abstract class BaseType {
  public abstract kind: string;
  public signature: string = "";
  public id = generateTypeIds ? nextTypeId++ : 0;

  constructor() {
    seenTypes.push(this as any as Type);
  }

  abstract copy(namedTypeCopies: Map<string, NamedType>): Type;

  abstract updateSignature(): void;
}

export class NameAndType {
  constructor(public readonly name: string, public type: Type) {}
}

let primitiveTypeId = 0;
export class PrimitiveType extends BaseType {
  public readonly kind: "primitive" = "primitive";

  constructor(public readonly name: string) {
    super();
    this.updateSignature();
    this.id = primitiveTypeId++;
  }

  copy(namedTypeCopies = new Map<string, NamedType>()): PrimitiveType {
    return this;
  }

  updateSignature(): void {
    this.signature = this.name;
  }
}

export class LiteralType extends BaseType {
  public readonly kind: "literal" = "literal";

  constructor(public readonly literal: string | number) {
    super();
    this.updateSignature();
  }

  copy(namedTypeCopies = new Map<string, NamedType>()): LiteralType {
    return this;
  }

  updateSignature(): void {
    this.signature = typeof this.literal === "string" ? `"${this.literal}"` : `${this.literal}`;
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

  copy(namedTypeCopies = new Map<string, NamedType>()): ListType {
    return new ListType(this.elementType.copy(namedTypeCopies));
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

  copy(namedTypeCopies = new Map<string, NamedType>()): MapType {
    return new MapType(this.valueType.copy(namedTypeCopies));
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

  copy(namedTypeCopies = new Map<string, NamedType>()): RecordType {
    return new RecordType(this.fields.map((field) => new NameAndType(field.name, field.type.copy(namedTypeCopies))));
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

  copy(namedTypeCopies = new Map<string, NamedType>()): FunctionType {
    const paramsCopy = this.parameters.map((param) => new NameAndType(param.name, param.type.copy(namedTypeCopies)));
    return new FunctionType(paramsCopy, this.returnType.copy(namedTypeCopies));
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

  copy(namedTypeCopies = new Map<string, NamedType>()): UnionType {
    return new UnionType(this.types.map((type) => type.copy(namedTypeCopies)));
  }

  updateSignature(): void {
    for (const type of this.types) {
      type.updateSignature();
    }
    this.signature =
      this.types.length == 1
        ? this.types[0].signature + "|"
        : this.types
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
    public genericTypes: NameAndType[],
    public isInstantiated: boolean,
    public type: Type,
    public ast: TypeNode,
    public readonly exported: boolean,
    public readonly location: SourceLocation
  ) {
    super();
    this.updateSignature();
  }

  copy(namedTypeCopies = new Map<string, NamedType>()): NamedType {
    // Named types should only be copied once when copying a type hierarchy,
    // so generic instantiation works as intended for recursive types like
    // type node[T] = <children: [node[T]], value: T>
    if (namedTypeCopies.has(this.name)) {
      return namedTypeCopies.get(this.name)!;
    }

    const copiedType = new NamedType(
      this.name,
      this.genericTypes.map((type) => new NameAndType(type.name, type.type)),
      this.isInstantiated,
      UnknownType,
      this.ast,
      this.exported,
      this.location
    );
    namedTypeCopies.set(this.name, copiedType);

    copiedType.type = this.type.copy(namedTypeCopies);
    return copiedType;
  }

  updateSignature(): void {
    if (this.updating) return;
    this.updating = true;
    let genericSignature: string[] = [];
    for (const genericType of this.genericTypes) {
      genericSignature.push(this.isInstantiated ? genericType.type.signature : genericType.name);
    }
    this.signature = this.name + (this.genericTypes.length > 0 ? "[" + genericSignature.join(",") + "]" : "");
    this.type.updateSignature();
    this.updating = false;
  }

  setGenericTypes(types: Map<string, Type>) {
    for (const genericType of this.genericTypes) {
      if (types.has(genericType.name)) {
        genericType.type = types.get(genericType.name)!;
      } else {
        throw new LittleFootError(this.location, `Internal error: couldn't find generic type binding for '${genericType.name}'.`);
      }
    }
  }

  getGenericType(name: string): Type {
    for (const genericType of this.genericTypes) {
      if (genericType.name == name) {
        return genericType.type;
      }
    }
    throw new LittleFootError(this.location, `Internal error: couldn't find generic type parameter '${name}'.`);
  }
}

export class NamedFunctionType extends BaseType {
  public readonly kind: "named function" = "named function";
  private updating = false;

  constructor(
    public readonly name: string,
    public genericTypes: NameAndType[],
    public isInstantiated: boolean,
    public type: FunctionType,
    public ast: FunctionNode,
    public readonly exported: boolean,
    public readonly external: boolean,
    public readonly location: SourceLocation
  ) {
    super();
    this.updateSignature();
  }

  copy(namedTypeCopies = new Map<string, NamedType>()): NamedFunctionType {
    return new NamedFunctionType(
      this.name,
      this.genericTypes.map((type) => new NameAndType(type.name, type.type)),
      this.isInstantiated,
      this.type.copy(namedTypeCopies),
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
    let genericSignature: string[] = [];
    for (const genericType of this.genericTypes) {
      genericSignature.push(this.isInstantiated ? genericType.type.signature : genericType.name);
    }
    this.signature = this.name + (this.genericTypes.length > 0 ? "[" + genericSignature.join(",") + "]" : "") + this.type.signature;
    this.updating = false;
  }

  signatureWithParameterNames() {
    let genericSignature: string[] = [];
    for (const genericType of this.genericTypes) {
      genericSignature.push(this.isInstantiated ? genericType.type.signature : genericType.name);
    }
    return (
      this.name +
      (this.genericTypes.length > 0 ? "[" + genericSignature.join(",") + "]" : "") +
      "(" +
      this.type.parameters.map((param) => param.name + ": " + param.type.signature).join(", ") +
      "): " +
      this.type.returnType.signature
    );
  }

  setGenericTypes(types: Map<string, Type>) {
    for (const genericType of this.genericTypes) {
      if (types.has(genericType.name)) {
        genericType.type = types.get(genericType.name)!;
      } else {
        throw new LittleFootError(this.location, `Internal error: couldn't find generic type binding for '${genericType.name}'.`);
      }
    }
  }

  getGenericType(name: string): Type {
    for (const genericType of this.genericTypes) {
      if (genericType.name == name) {
        return genericType.type;
      }
    }
    throw new LittleFootError(this.location, `Internal error: couldn't find generic type parameter '${name}'.`);
  }
}

export type Type = PrimitiveType | LiteralType | ListType | MapType | RecordType | FunctionType | UnionType | NamedType | NamedFunctionType;
export const UnknownType = new PrimitiveType("$unknown");
export const NothingType = new PrimitiveType("nothing");
export const BooleanType = new PrimitiveType("boolean");
export const Int8Type = new PrimitiveType("int8");
export const Int16Type = new PrimitiveType("int16");
export const Int32Type = new PrimitiveType("int32");
export const Int64Type = new PrimitiveType("int64");
export const Float32Type = new PrimitiveType("float32");
export const Float64Type = new PrimitiveType("float64");
export const NumberType = Float64Type;
export const StringType = new PrimitiveType("string");
export const AnyType = new PrimitiveType("$any");
export const ResolvingTypeMarker = new PrimitiveType("$resolving");
export const primitiveTypes = [NothingType, BooleanType, Int8Type, Int16Type, Int32Type, Int64Type, Float64Type, NumberType, StringType];
nextTypeId = ResolvingTypeMarker.id + 2;

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
    case "literal":
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

  // This needs to catch things like:
  //
  // func foo[Z](v: Z): nothing
  // func foo[Z](v: Z): number
  //
  // or
  //
  // func foo[Z](v: Z): nothing
  // func foo[V](v: V): nothing
  findDuplicate(func: NamedFunctionType) {
    let funcs = this.lookup.get(func.name);
    if (!funcs) return null;
    funcs = funcs.filter(
      (other) => func.genericTypes.length == other.genericTypes.length && func.type.parameters.length == other.type.parameters.length
    );
    if (funcs.length == 0) return null;
    for (const other of funcs) {
      let mismatch = false;
      for (let i = 0; i < other.type.parameters.length; i++) {
        const otherParam = other.type.parameters[i];
        const param = func.type.parameters[i];
        if (!isEqual(otherParam.type, param.type)) {
          mismatch = true;
          break;
        }
      }
      if (!mismatch) {
        return other;
      }
    }
    return null;
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
    const duplicate = this.findDuplicate(func);
    if (duplicate) {
      // Adding the exact same function is allowed if the location matches so
      // module import handling is easier.
      if (func.location.equals(duplicate.location)) {
        return;
      }
      throw new LittleFootError(
        func.ast.name.location,
        `Duplicate function '${func.signatureWithParameterNames()}', first defined in ${duplicate.location.toString()}.`
      );
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
    this.add(Int8Type.name, Int8Type);
    this.add(Int16Type.name, Int16Type);
    this.add(Int32Type.name, Int32Type);
    this.add(Int64Type.name, Int64Type);
    this.add(Float32Type.name, Float32Type);
    this.add(Float64Type.name, Float64Type);
    this.add("number", NumberType);
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
          if (type.location.equals(otherType.location)) {
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
  // If both are named types, we need to cover a few cases
  if (from.kind == "named type" && to.kind == "named type") {
    // If both are generic any types, they are considered to be the same
    if (from.type == AnyType && to.type == AnyType) return true;

    // Identity check: If they aren't at the same location, they aren't equal.
    if (!from.location.equals(to.location)) return false;

    // Non-generic check: If they don't have generic type parameters, they are equal
    // We know they are from the same location, so we could only check one.
    if (from.genericTypes.length == 0 && to.genericTypes.length == 0) return true;

    // Generic instance check: If only one is instantiated, they aren't equal.
    // We know they are the same generic type
    if ((from.isInstantiated && !to.isInstantiated) || (!from.isInstantiated && to.isInstantiated)) return false;

    // Otherwise, they are both generic instantiated types. We must compare their generic type bindings
    // for equality. We can assume their generic type count to be the same, as they are from
    // the same location.
    for (let i = 0; i < from.genericTypes.length; i++) {
      if (!isEqual(from.genericTypes[i].type, to.genericTypes[i].type)) return false;
    }
    return true;
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

  // Literal types must match by literal exactly.
  if (from.kind == "literal" && to.kind == "literal") {
    return from.literal == to.literal;
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
  // degenerate case can arise (e.g. when mixins are involved, or generics like T | number like
  // type bar[T] = T | number; var b: bar[number] = 0):
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

export function rawType(type: Type) {
  let rawType = type;
  while (rawType.kind == "named function" || rawType.kind == "named type") {
    rawType = rawType.type;
  }
  return rawType;
}

// If from is a List and map with element/valueType equal to UnknownType,
// its element/valueType will be set to `to`'s element/valueType
// and it will be reported to be assignable. This allows empty
// list and map literals to be assigned to variables, fields,
// function arguments and so on.
export function isAssignableTo(from: Type, literal: string | number | undefined, to: Type): boolean {
  // If to is the any type, anything is assignable
  if (rawType(to) == AnyType) {
    return true;
  }

  // If from is the any type, is is assignable to anything
  if (rawType(from) == AnyType) {
    return true;
  }

  // If both types are named types with the same name, then they are only
  // equal if they are the same type by identity.
  // This is needed to stop the recursion for types like
  // type node = <children: [node], value: number>
  if (from.kind == "named type" && to.kind == "named type" && from.name == to.name) {
    return from.location.equals(to.location);
  }

  // Unpack the type of named types
  from = rawType(from);
  to = rawType(to);

  // This handles primitives and is also an early out for
  // exact type matches. This also handles exact record matches
  // Non-exact matches are handled below.
  if (isEqual(from, to)) return true;

  // If to is a literal, the from literal must match
  if (to.kind == "literal") return to.literal == literal;

  // If to is a string and from is a literal, it must be a string literal type
  if (to == StringType && literal) return typeof literal === "string";

  // If to is a number and from is a literal, it must be a number literal that fits
  // the size of to's numeric type.
  if (to == NumberType && literal) return typeof literal === "number";

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
        if (isAssignableTo(from, literal, type)) return true;
      }
      return false;
    } else {
      for (const aType of from.types) {
        let found = false;
        for (const bType of to.types) {
          if (isAssignableTo(aType, literal, bType)) {
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
    if (rawType(to.elementType).kind == "union" && rawType(from.elementType).kind != "union") {
      return false;
    }
    return isAssignableTo(from.elementType, literal, to.elementType);
  }

  // For maps, if the to value type is a union then
  // the from value type must be a union as well, so we honor memory layouts.
  // E.g. {"a": number} can not be assigned to {"a": number | string}.
  // The from element type must be assignable to the to element type
  if (from.kind == "map" && to.kind == "map") {
    if (rawType(to.valueType).kind == "union" && rawType(from.valueType).kind != "union") {
      return false;
    }
    return isAssignableTo(from.valueType, literal, to.valueType);
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
    for (const toField of to.fields) {
      let found = false;
      for (const fromField of from.fields) {
        if (fromField.name !== toField.name) continue;

        // If the to field is a union and the from field is not, the
        // from is not assignable to. This is required to honor memory layouts.
        if (rawType(toField.type).kind == "union" && rawType(fromField.type).kind != "union") {
          return false;
        }

        // Otherwise, from needs to be assignable to to.
        if (isAssignableTo(fromField.type, literal, toField.type)) {
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
      // If the to parameter is a union and the from parameter is not, the
      // from is not assignable to. This is required to honor memory layouts.
      if (rawType(to.parameters[i].type).kind == "union" && rawType(from.parameters[i].type).kind != "union") {
        return false;
      }

      if (!isAssignableTo(from.parameters[i].type, literal, to.parameters[i].type)) return false;
    }
    if (!isAssignableTo(from.returnType, literal, to.returnType)) return false;
    return true;
  }

  // Otherwise, the from is not assignable to to
  return false;
}

export function isGeneric(type: Type) {
  let found = false;
  traverseType(type, (type) => {
    if (type == AnyType) {
      found = true;
      return false;
    }
    return true;
  });
  return found;
}

export function hasEmptyListOrMap(type: Type) {
  let found = false;
  traverseType(type, (type) => {
    if (type.kind == "list" && type.elementType.signature == UnknownType.signature) {
      found = true;
      return false;
    }
    if (type.kind == "map" && type.valueType.signature == UnknownType.signature) {
      found = true;
      return false;
    }
    return true;
  });
  return found;
}

export function hasUnion(type: Type) {
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

export function isRecursive(type: NamedType, errors: LittleFootError[]): boolean {
  // FIXME this breaks V8 due to infinite "recursion" for
  const startErrors = errors.length;
  let seenTypes: Type[] = [];
  let typesToVisit: Type[] = [];
  typesToVisit.push(type.type);
  seenTypes.push(type);
  while (typesToVisit.length > 0) {
    const currType = typesToVisit.pop();
    if (currType == null) break;
    seenTypes.push(currType);

    if (currType.kind == "named type") {
      typesToVisit.push(currType.type);
      if (currType == type) {
        errors.push(
          new LittleFootError(
            type.ast.name.location,
            `Type '${type.name}' circularly references itself.`,
            `Chain: ${seenTypes.map((t) => t.signature).join(" -> ")}`
          )
        );
        break;
      }
    } else {
      if (currType.kind == "union") {
        for (const unionType of currType.types) {
          if (unionType == type) {
            errors.push(
              new LittleFootError(
                type.ast.name.location,
                `Type '${type.name}' circularly references itself.`,
                `Chain: ${seenTypes.map((t) => t.signature).join(" -> ")}`
              )
            );
            break;
          }
          if (unionType.kind == "named type") {
            typesToVisit.push(unionType);
          }
        }
      } else {
        break;
      }
    }
  }
  return startErrors != errors.length;
}

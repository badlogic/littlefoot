import { AstNode, FunctionLiteralNode, FunctionNode, TypeNode } from "./ast";
import { LittleFootError } from "./error";
import { SourceLocation } from "./source";

export abstract class BaseType {
  public abstract kind: string;
  constructor(public signature: string) {}

  abstract copy(): Type;
}

export class NameAndType {
  constructor(public readonly name: string, public type: Type) {}
}

export class PrimitiveType extends BaseType {
  public readonly kind: "primitive" = "primitive";

  constructor(public readonly name: string) {
    super(name);
  }

  copy(): PrimitiveType {
    return new PrimitiveType(this.name);
  }
}

export class ListType extends BaseType {
  public readonly kind: "list" = "list";

  constructor(public elementType: Type) {
    super("[" + elementType.signature + "]");
  }

  setElementType(elementType: Type) {
    this.elementType = elementType;
    this.signature = "[" + elementType.signature + "]";
  }

  copy(): ListType {
    return new ListType(this.elementType.copy());
  }
}

export class MapType extends BaseType {
  public readonly kind: "map" = "map";

  constructor(public valueType: Type) {
    super("{" + valueType.signature + "}");
  }

  setValueType(valueType: Type) {
    this.valueType = valueType;
    this.signature = "{" + valueType.signature + "}";
  }

  copy(): MapType {
    return new MapType(this.valueType.copy());
  }
}

export class RecordType extends BaseType {
  public readonly kind: "record" = "record";

  constructor(public readonly fields: NameAndType[]) {
    super(
      "<" +
        fields
          .map((field) => field.name + ":" + field.type.signature)
          .sort()
          .join(",") +
        ">"
    );
    fields.sort(); // needed for equality and assignability checks
  }

  updateSignature() {
    this.signature =
      "<" +
      this.fields
        .map((field) => field.name + ":" + field.type.signature)
        .sort()
        .join(",") +
      ">";
  }

  copy(): RecordType {
    return new RecordType(this.fields.map((field) => new NameAndType(field.name, field.type.copy())));
  }
}

export class FunctionType extends BaseType {
  public readonly kind: "function" = "function";

  constructor(public readonly parameters: NameAndType[], public returnType: Type) {
    super("(" + parameters.map((param) => param.type.signature).join(",") + "):" + returnType.signature);
  }

  setReturnType(returnType: Type) {
    this.returnType = returnType;
    this.signature = "(" + this.parameters.map((param) => param.type.signature).join(",") + "):" + returnType.signature;
  }

  copy(): FunctionType {
    const paramsCopy = this.parameters.map((param) => new NameAndType(param.name, param.type.copy()));
    return new FunctionType(paramsCopy, this.returnType.copy());
  }
}

export class UnionType extends BaseType {
  public readonly kind: "union" = "union";

  constructor(public readonly types: Type[]) {
    super(
      types
        .map((type) => type.signature)
        .sort()
        .join("|")
    );
  }

  copy(): UnionType {
    return new UnionType(this.types.map((type) => type.copy()));
  }
}

export class NamedType extends BaseType {
  public readonly kind: "named type" = "named type";

  constructor(public readonly name: string, public type: Type, public typeNode: TypeNode, public readonly location: SourceLocation) {
    super(name);
  }

  copy(): NamedType {
    return new NamedType(this.name, this.type.copy(), this.typeNode, this.location);
  }
}

export class NamedFunctionType extends BaseType {
  public readonly kind: "named function" = "named function";

  constructor(
    public readonly name: string,
    public type: FunctionType,
    public ast: FunctionNode | FunctionLiteralNode,
    public readonly exported: boolean,
    public readonly external: boolean,
    public readonly location: SourceLocation
  ) {
    super(name + type.signature);
  }

  updateReturnType() {
    this.type = this.ast.type as FunctionType;
    this.signature = this.name + this.type.signature;
  }

  copy(): NamedFunctionType {
    return new NamedFunctionType(this.name, this.type.copy(), this.ast, this.exported, this.external, this.location);
  }
}

export type Type = PrimitiveType | ListType | MapType | RecordType | FunctionType | UnionType | NamedType | NamedFunctionType;

export const NothingType = new PrimitiveType("nothing");
export const BooleanType = new PrimitiveType("boolean");
export const NumberType = new PrimitiveType("number");
export const StringType = new PrimitiveType("string");
export const UnknownType = new PrimitiveType("$unknown");
export const ResolvingTypeMarker = new PrimitiveType("$resolving");

function assertNever(x: never) {
  throw new Error("Unexpected object: " + x);
}

export function traverseType(type: Type, callback: (type: Type) => boolean) {
  if (!callback(type)) return;
  switch (type.kind) {
    case "function":
      for (const parameter of type.parameters) {
        traverseType(parameter.type, callback);
      }
      traverseType(type.returnType, callback);
      break;
    case "primitive":
      break;
    case "list":
      traverseType(type.elementType, callback);
      break;
    case "map":
      traverseType(type.valueType, callback);
      break;
    case "record":
      for (const field of type.fields) {
        traverseType(field.type, callback);
      }
      break;
    case "union":
      for (const unionType of type.types) {
        traverseType(unionType, callback);
      }
      break;
    case "named type":
      traverseType(type.type, callback);
      break;
    case "named function":
      traverseType(type.type, callback);
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
      const otherType = this.getExact(name, func.signature)! as NamedFunctionType;
      throw new LittleFootError(func.location, `Duplicate function '${name}', first defined in ${otherType.location.toString()}.`);
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
    this.add(NothingType);
    this.add(BooleanType);
    this.add(NumberType);
    this.add(StringType);
    this.add(UnknownType);
  }

  get(name: string) {
    return this.lookup.get(name);
  }

  has(name: string) {
    return this.lookup.has(name);
  }

  add(type: PrimitiveType | NamedType) {
    if (this.has(type.name)) {
      const otherType = this.get(type.name)!;
      if (otherType.kind == "primitive") {
        if (type.kind != "primitive")
          throw new LittleFootError(type.location, `Can not use '${type.name}' as a type name, as a built-in type with that name exists.`);
        else throw new Error("Tried to add a built-in type twice");
      } else {
        if (type.kind != "primitive") {
          throw new LittleFootError(type.location, `Duplicate type '${type.name}', first defined in ${otherType.location.toString()}.`);
        } else {
          throw new Error("Tried to add a built-in type twice");
        }
      }
    } else {
      this.lookup.set(type.name, type);
    }
  }
}

export function isEqual(from: Type, to: Type) {
  // Unpack the type of named types.
  if (from.kind == "named function" || from.kind == "named type") {
    from = from.type;
  }
  if (to.kind == "named function" || to.kind == "named type") {
    to = to.type;
  }

  if (from.kind == "primitive" && to.kind == "primitive") {
    // Primitive types must match by name exactly.
    return from.name == to.name;
  } else if (from.kind == "list" && to.kind == "list") {
    // List types must have equal element types.
    return isEqual(from.elementType, to.elementType);
  } else if (from.kind == "map" && to.kind == "map") {
    // Map types must have equal value types.
    return isEqual(from.valueType, to.valueType);
  } else if (from.kind == "record" && to.kind == "record") {
    // Records must have the same number of fields. Each
    // pair of fields must match in both name and type.
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
  } else if (from.kind == "union" && to.kind == "union") {
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
  } else if (from.kind == "function" && to.kind == "function") {
    // Functions must have the same number of parameters. Each
    // parameter pair must have equal types. The return types
    // must also be equal. The parameter names
    // are irrelevant for equality.
    if (from.parameters.length != to.parameters.length) return false;
    for (let i = 0; i < from.parameters.length; i++) {
      if (!isEqual(from.parameters[i].type, to.parameters[i].type)) return false;
    }
    if (!isEqual(from.returnType, to.returnType)) return false;
    return true;
  } else {
    return false;
  }
}

// If from is a List and map with element/valueType equal to UnknownType,
// its element/valueType will be set to `to`'s element/valueType
// and it will be reported to be assignable. This allows empty
// list and map literals to be assigned to variables, fields,
// function arguments and so on.
export function isAssignableTo(from: Type, to: Type): boolean {
  if (from.kind == "named function" || from.kind == "named type") {
    from = from.type;
  }
  if (to.kind == "named function" || to.kind == "named type") {
    to = to.type;
  }

  // This handles primitives and is also an early out for
  // exact type matches. This also handles exact record matches
  // Non-exact matches are handled below.
  if (isEqual(from, to)) return true;

  if (from.kind == "union" && to.kind != "union") {
    // If `to` is not a union, then all types in `from` must be assignable to `to`.
    for (const type of from.types) {
      if (!isAssignableTo(type, to)) return true;
    }
    return true;
  } else if (to.kind == "union") {
    // If `from` is a union and `to` is not, it can not be assigned.
    // This is why we start by checking if `to` is a union.
    //
    // There are two cases:
    // 1. If `from` is not a union, then it must be assignable
    //    to at least one type in the `to` union.
    // 2. If `from` is a union, then all of `from`'s types must be
    //    assignable to at least one type in `to`.
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
  } else if (from.kind == "list" && to.kind == "list") {
    // For lists, the element types must be assignable
    // for records, and equal for everything else.
    return isAssignableTo(from.elementType, to.elementType);
  } else if (from.kind == "map" && to.kind == "map") {
    // For maps, the element types must be assignable
    // for records, and equal for everything else.
    return isAssignableTo(from.valueType, to.valueType);
  } else if (from.kind == "record" && to.kind == "record") {
    // We only get here if isEqual(from, to) was false, which means
    // the two record types don't have the same number or exact
    // types of fields.
    //
    // However, if we can assign all fields of from to a subset of fields
    // of record to, we can assign from to to.
    if (from.fields.length < to.fields.length) return false;
    let matchedFields = 0;
    for (const fromField of from.fields) {
      let found = false;
      for (const toField of to.fields) {
        if (fromField.name !== toField.name) continue;
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
  } else if (from.kind == "function" && to.kind == "function") {
    // For functions, the number of parameters must match,
    // and the parameter types of `from` must be assignable to
    // the parameter types of `. The return type of a must
    // also be assignable to b. The parameter names do not matter.
    if (from.parameters.length != to.parameters.length) return false;
    for (let i = 0; i < from.parameters.length; i++) {
      if (!isAssignableTo(from.parameters[i].type, to.parameters[i].type)) return false;
    }
    if (!isAssignableTo(from.returnType, to.returnType)) return false;
    return true;
  } else {
    return false;
  }
}

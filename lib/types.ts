import { FunctionNode, TypeNode } from "./ast";

export abstract class BaseType {
  private _resolvedSignature: string | null = null;

  constructor(public readonly signature: string) {}

  get resolvedSignature() {
    if (!this._resolvedSignature) {
      this._resolvedSignature = this.resolveSignature();
    }
    return this._resolvedSignature;
  }
  protected abstract resolveSignature(): string;
}

export class NameAndType {
  constructor(public readonly name: string, public type: Type) {}
}

export class PrimitiveType extends BaseType {
  public readonly kind: "primitive" = "primitive";

  constructor(public readonly name: string) {
    super(name);
  }

  protected resolveSignature(): string {
    return this.name;
  }
}

export class ListType extends BaseType {
  public readonly kind: "list" = "list";

  constructor(public readonly elementType: Type) {
    super("[" + elementType.signature + "]");
  }

  protected resolveSignature(): string {
    return "[" + this.elementType.resolvedSignature + "]";
  }
}

export class MapType extends BaseType {
  public readonly kind: "map" = "map";

  constructor(public readonly valueType: Type) {
    super("{" + valueType.signature + "}");
  }

  protected resolveSignature(): string {
    return "[" + this.valueType.resolvedSignature + "]";
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

  protected resolveSignature(): string {
    return (
      "<" +
      this.fields
        .map((field) => field.name + ":" + field.type.resolvedSignature)
        .sort()
        .join(",") +
      ">"
    );
  }
}

export class FunctionType extends BaseType {
  public readonly kind: "function" = "function";

  constructor(public readonly parameters: NameAndType[], public returnType: Type) {
    super("(" + parameters.map((param) => param.type.signature).join(",") + "):" + returnType.signature);
  }

  protected resolveSignature(): string {
    return "(" + this.parameters.map((param) => param.type.resolvedSignature).join(",") + "):" + this.returnType.resolvedSignature;
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

  protected resolveSignature(): string {
    return this.types
      .map((type) => type.resolvedSignature)
      .sort()
      .join("|");
  }
}

export class NamedType extends BaseType {
  public readonly kind: "named type" = "named type";

  constructor(public readonly name: string, public type: Type, public node: TypeNode) {
    super(name);
  }

  protected resolveSignature(): string {
    return this.type.resolvedSignature;
  }
}

export class NamedFunction extends BaseType {
  public readonly kind: "named function" = "named function";

  constructor(public readonly name: string, public type: FunctionType, public node: FunctionNode) {
    super(name + type.signature);
  }

  protected resolveSignature(): string {
    return this.type.resolvedSignature;
  }
}

export type Type = PrimitiveType | ListType | MapType | RecordType | FunctionType | UnionType | NamedType | NamedFunction;

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

export class Types {
  public readonly allTypes = new Map<String, PrimitiveType | NamedType | NamedFunction>();

  constructor() {
    this.add(NothingType);
    this.add(BooleanType);
    this.add(NumberType);
    this.add(StringType);
    this.add(UnknownType);
  }

  get(name: string) {
    return this.allTypes.get(name);
  }

  has(signature: string) {
    return this.allTypes.has(signature);
  }

  add(type: PrimitiveType | NamedType | NamedFunction) {
    if (this.allTypes.has(type.signature)) {
      throw new Error(`Internal compiler error: Type ${type.kind} -> ${type.signature} already exists`);
    }
    this.allTypes.set(type.signature, type);
  }
}

function isEqual(a: Type, b: Type) {
  // Unpack the type of named types.
  if (a.kind == "named function" || a.kind == "named type") {
    a = a.type;
  }
  if (b.kind == "named function" || b.kind == "named type") {
    b = b.type;
  }

  if (a.kind == "primitive" && b.kind == "primitive") {
    // Primitive types must match by name exactly.
    return a.name == b.name;
  } else if (a.kind == "list" && b.kind == "list") {
    // List types must have equal element types.
    return isEqual(a.elementType, b.elementType);
  } else if (a.kind == "map" && b.kind == "map") {
    // Map types must have equal value types.
    return isEqual(a.valueType, b.valueType);
  } else if (a.kind == "record" && b.kind == "record") {
    // Records must have the same number of fields. Each
    // pair of fields must match in both name and type.
    if (a.fields.length != b.fields.length) return false;
    for (let i = 0; i < a.fields.length; i++) {
      const aField = a.fields[i];
      const bField = b.fields[i];
      if (aField.name != bField.name) return false;
      if (!isEqual(aField.type, bField.type)) return false;
    }
    return true;
  } else if (a.kind == "union" && b.kind == "union") {
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
    if (a.types.length != b.types.length) return false;
    for (let i = 0; i < a.types.length; i++) {
      const aType = a.types[i];
      let found = false;
      for (let j = 0; j < b.types.length; j++) {
        const bType = b.types[j];
        if (isEqual(aType, bType)) {
          found = true;
          break;
        }
      }
      if (!found) return false;
    }
    return true;
  } else if (a.kind == "function" && b.kind == "function") {
    // Functions must have the same number of parameters. Each
    // parameter pair must have equal types. The return types
    // must also be equal. The parameter names
    // are irrelevant for equality.
    if (a.parameters.length != b.parameters.length) return false;
    for (let i = 0; i < a.parameters.length; i++) {
      if (!isEqual(a.parameters[i].type, b.parameters[i].type)) return false;
    }
    if (!isEqual(a.returnType, b.returnType)) return false;
    return true;
  } else {
    return false;
  }
}

function isAssignableTo(a: Type, b: Type) {
  if (a.kind == "named function" || a.kind == "named type") {
    a = a.type;
  }
  if (b.kind == "named function" || b.kind == "named type") {
    b = b.type;
  }

  // This handles primitives and is also an early out for
  // exact type matches. This also handles exact record matches
  // Non-exact matches are handled below.
  if (isEqual(a, b)) return true;

  if (b.kind == "union") {
    // If a is a union and b is not, it can not be assigned.
    // This is why we start by check if b is a union.
    //
    // There are two cases:
    // 1. If a is not a union, then it must be assignable
    //    to at least one type in the union.
    // 2. If a is a union, then all of a's types must be
    //    assignable to at least one type in b.
    if (a.kind != "union") {
      for (const type of b.types) {
        if (isAssignableTo(type, a)) return true;
      }
      return false;
    } else {
      for (const aType of a.types) {
        let found = false;
        for (const bType of b.types) {
          if (isAssignableTo(aType, bType)) {
            found = true;
            break;
          }
        }
        if (!found) return false;
      }
      return true;
    }
  } else if (a.kind == "list" && b.kind == "list") {
    // For lists, the element type of a must be assignable to
    // element type of b.
    return isAssignableTo(a.elementType, b.elementType);
  } else if (a.kind == "map" && b.kind == "map") {
    // For maps, the value type of a must be assignable to
    // value types of b.
    return isAssignableTo(a.valueType, b.valueType);
  } else if (a.kind == "record" && b.kind == "record") {
    // We only get here if isEqual(a, b) was false, which means
    // the two record types don't have the same number or exact
    // types of fields.
    //
    // However, if we can assign all fields of a to a subset of fields
    // of record b, we can assign a to b.
    for (const aField of a.fields) {
      let found = false;
      for (const bField of b.fields) {
        if (isAssignableTo(aField.type, bField.type)) {
          found = true;
          break;
        }
      }
      if (!found) return false;
    }
  } else if (a.kind == "function" && b.kind == "function") {
    // For functions, the number of parameters must match,
    // and the parameter types of a must be assignable to
    // the parameter types of b. The return type of a must
    // also be assignable to b. The parameter names do not matter.
    if (a.parameters.length != b.parameters.length) return false;
    for (let i = 0; i < a.parameters.length; i++) {
      if (!isAssignableTo(a.parameters[i].type, b.parameters[i].type)) return false;
    }
    if (!isAssignableTo(a.returnType, b.returnType)) return false;
    return true;
  } else {
    return false;
  }
}

type a = (a: number) => string | number;
type b = (a: number) => string;

type AreEqual<T, U> = T extends U ? (U extends T ? true : false) : false;

type typesEqual = AreEqual<a, b>;

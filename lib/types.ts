import { FunctionLiteralNode, FunctionNode, TypeNode } from "./ast";

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

export class ArrayType extends BaseType {
  public readonly kind: "array" = "array";

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

export type Type = PrimitiveType | ArrayType | MapType | RecordType | FunctionType | UnionType | NamedType | NamedFunction;

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
    case "array":
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

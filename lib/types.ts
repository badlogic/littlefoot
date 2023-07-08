import { FunctionNode, RecordNode } from "./ast";

export abstract class BaseType {
  constructor(public readonly signature: string) {}
}

export class NameAndType {
  constructor(public readonly name: string, public readonly type: BaseType) {}
}

export class PrimitiveType extends BaseType {
  public readonly kind: "primitive" = "primitive";

  constructor(public readonly name: string) {
    super(name);
  }
}

export class ArrayType extends BaseType {
  public readonly kind: "array" = "array";

  constructor(public readonly elementTypes: BaseType[]) {
    super("[" + elementTypes.map((type) => type.signature).join("|") + "]");
  }
}

export class MapType extends BaseType {
  public readonly kind: "map" = "map";

  constructor(public readonly valueTypes: BaseType[]) {
    super("{" + valueTypes.map((type) => type.signature).join("|") + "}");
  }
}

export class TupleType extends BaseType {
  public readonly kind: "tuple" = "tuple";

  constructor(public readonly fields: NameAndType[]) {
    super("<" + fields.map((field) => field.name + ":" + field.type.signature).join(",") + ">");
  }
}

export class FunctionType extends BaseType {
  public readonly kind: "function" = "function";

  constructor(public readonly parameters: NameAndType[], public readonly returnType: BaseType, public readonly node: FunctionNode | null) {
    super("(" + parameters.map((param) => param.type.signature).join(",") + "):" + returnType.signature);
  }
}

export class RecordType extends BaseType {
  public readonly kind: "record" = "record";

  constructor(public readonly name: string, public readonly fields: NameAndType[], public readonly node: RecordNode) {
    super(name);
  }
}

export class UnionType extends BaseType {
  public readonly kind: "union" = "union";

  constructor(public readonly types: BaseType[]) {
    super(types.map((type) => type.signature).join("|"));
  }
}

export class AliasType extends BaseType {
  public readonly kind: "alias" = "alias";

  constructor(public readonly name: string, public readonly type: BaseType) {
    super(name);
  }
}

export type Type = PrimitiveType | ArrayType | MapType | TupleType | FunctionType | RecordType | UnionType | AliasType;

export const NothingType = new PrimitiveType("nothing");
export const BooleanType = new PrimitiveType("boolean");
export const NumberType = new PrimitiveType("number");
export const StringType = new PrimitiveType("string");

export class Types {
  public readonly allTypes = new Map<String, Type>();

  constructor() {
    this.add(NothingType);
    this.add(BooleanType);
    this.add(NumberType);
    this.add(StringType);
  }

  get(signature: string) {
    return this.allTypes.get(signature);
  }

  add(type: Type) {
    if (this.allTypes.has(type.signature)) {
      throw new Error(`Internal compiler error: Type ${type.kind} -> ${type.signature} already exists`);
    }
    this.allTypes.set(type.signature, type);
  }
}

import * as fs from "fs";
import { Source, parse } from "../lib";
import { checkTypes } from "../lib/typechecker";
import { ArrayType, MapType, NameAndType, NamedType, NothingType, NumberType, StringType, TupleType, Types, UnionType } from "../lib/types";

describe("Typechecker tests", () => {
  it("Should error if tuples of mixin share field names", () => {
    const { ast, errors } = parse(
      new Source(
        "source.lf",
        `
          type a = <x: number>
          type b = a + <y: number> + <x: string>
        `
      )
    );

    const types = new Types();
    checkTypes(ast, errors, types);
    expect(errors.length).toBe(1);
  });

  it("Should error if not all types of a mixin are tuples", () => {
    const { ast, errors } = parse(
      new Source(
        "source.lf",
        `
          type a = <x: number>
          type b = a + <y: number> + number
        `
      )
    );

    const types = new Types();
    checkTypes(ast, errors, types);
    expect(errors.length).toBe(1);
  });

  it("Should error on circular types", () => {
    const { ast, errors } = parse(
      new Source(
        "source.lf",
        `
          type a = number | b
          type b = a | number
        `
      )
    );

    const types = new Types();
    checkTypes(ast, errors, types);
    expect(errors.length).toBe(2);
    expect(errors[0].message == "Type 'a' circularly references itself.").toBe(true);
    expect(errors[1].message == "Type 'b' circularly references itself.").toBe(true);
  });

  it("Should validate simple named types", () => {
    const { ast, errors } = parse(
      new Source(
        "source.lf",
        `
        type a = nothing
        type b = number
        type c = [number]
        type d = {number}
        type e = <x: number, y: string>
        type u = a | b | nothing
    `
      )
    );
    const types = new Types();
    checkTypes(ast, errors, types);
    expect(errors.length).toBe(0);

    const a = types.get("a")! as NamedType;
    expect(a.kind).toBe("named type");
    expect(a.type).toStrictEqual(NothingType);

    const b = types.get("b")! as NamedType;
    expect(b.kind).toBe("named type");
    expect(b.type).toStrictEqual(NumberType);

    const c = types.get("c")! as NamedType;
    expect(c.kind).toBe("named type");
    expect(c.type).toStrictEqual(new ArrayType(NumberType));

    const d = types.get("d")! as NamedType;
    expect(d.kind).toBe("named type");
    expect(d.type).toStrictEqual(new MapType(NumberType));

    const e = types.get("e")! as NamedType;
    expect(e.kind).toBe("named type");
    expect(e.type).toStrictEqual(new TupleType([new NameAndType("x", NumberType), new NameAndType("y", StringType)]));

    const u = types.get("u")! as NamedType;
    expect(u.kind).toBe("named type");
    expect(u.type.kind).toBe("union");
    expect((u.type as UnionType).types[0]).toStrictEqual(a);
    expect((u.type as UnionType).types[1]).toStrictEqual(b);
    expect((u.type as UnionType).types[2]).toStrictEqual(NothingType);
  });

  it("Shouldn't allow usage of built-in type names for named types", () => {
    const { ast, errors } = parse(
      new Source(
        "source.lf",
        `
        type string = number
    `
      )
    );
    const types = new Types();
    checkTypes(ast, errors, types);
    expect(errors.length).toBe(1);
  });

  it("Shouldn't allow duplicate named types", () => {
    const { ast, errors } = parse(
      new Source(
        "source.lf",
        `
        type a = number
        type a = number
    `
      )
    );
    const types = new Types();
    checkTypes(ast, errors, types);
    expect(errors.length).toBe(1);
  });
});

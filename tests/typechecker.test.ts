import * as fs from "fs";
import { Source, parse } from "../lib";
import { checkTypes } from "../lib/typechecker";
import { ListType, MapType, NameAndType, NamedType, NothingType, NumberType, StringType, RecordType, Types, UnionType } from "../lib/types";
import { compile } from "../lib/compiler";

describe("Typechecker tests", () => {
  it("Should infer type from initializer", () => {
    const { modules, errors } = compile(
      "source.lf",
      (path) =>
        new Source(
          "source.lf",
          `
          var a = nothing
          var b = true
          var c = 1.123
          var d = "Hello"
        `
        )
    );
    expect(errors.length).toBe(0);
  });

  it("Should error if records of mixin share field names", () => {
    const { errors } = compile(
      "source.lf",
      (path) =>
        new Source(
          "source.lf",
          `
          type a = <x: number>
          type b = a + <y: number> + <x: string>
        `
        )
    );
    expect(errors.length).toBe(1);
  });

  it("Should type check complex types", () => {
    const { errors } = compile(
      "source.lf",
      (path) =>
        new Source(
          "source.lf",
          `
          type shapes = rectangle | circle | colored + <width: number>
          type color = <r: number, g: number, b: number>
          type colored = <color: color>
          type rectangle = colored + <width: number, height: number> # mixins!
          type circle = colored + <radius: number>
        `
        )
    );
    expect(errors.length).toBe(0);
  });

  it("Should error if not all types of a mixin are records", () => {
    const { errors } = compile(
      "source.lf",
      (path) =>
        new Source(
          "source.lf",
          `
          type a = <x: number>
          type b = a + <y: number> + number
        `
        )
    );
    expect(errors.length).toBe(1);
    expect(errors[0].message).toEqual("All types in a mixin must be a record.");
  });

  it("Should error on circular types", () => {
    const { errors } = compile(
      "source.lf",
      (path) =>
        new Source(
          "source.lf",
          `
          type a = number | b
          type b = a | number
        `
        )
    );

    expect(errors.length).toBe(1);
    expect(errors[0].message).toEqual("Type 'b' circularly references itself.");
  });

  it("Should validate simple named types", () => {
    const { types, errors } = compile(
      "source.lf",
      (path) =>
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

    expect(errors.length).toBe(0);

    const a = types.get("a")! as NamedType;
    expect(a.kind).toBe("named type");
    expect(a.type).toStrictEqual(NothingType);

    const b = types.get("b")! as NamedType;
    expect(b.kind).toBe("named type");
    expect(b.type).toStrictEqual(NumberType);

    const c = types.get("c")! as NamedType;
    expect(c.kind).toBe("named type");
    expect(c.type).toStrictEqual(new ListType(NumberType));

    const d = types.get("d")! as NamedType;
    expect(d.kind).toBe("named type");
    expect(d.type).toStrictEqual(new MapType(NumberType));

    const e = types.get("e")! as NamedType;
    expect(e.kind).toBe("named type");
    expect(e.type).toStrictEqual(new RecordType([new NameAndType("x", NumberType), new NameAndType("y", StringType)]));

    const u = types.get("u")! as NamedType;
    expect(u.kind).toBe("named type");
    expect(u.type.kind).toBe("union");
    expect((u.type as UnionType).types[0]).toStrictEqual(a);
    expect((u.type as UnionType).types[1]).toStrictEqual(b);
    expect((u.type as UnionType).types[2]).toStrictEqual(NothingType);
  });

  it("Shouldn't allow usage of built-in type names for named types", () => {
    const { errors } = compile(
      "source.lf",
      (path) =>
        new Source(
          "source.lf",
          `
          type string = number
        `
        )
    );
    expect(errors.length).toBe(1);
    expect(errors[0].message).toEqual("Can not use 'string' as a type name, as a built-in type with that name exists.");
  });

  it("Shouldn't allow duplicate named types", () => {
    const { errors } = compile(
      "source.lf",
      (path) =>
        new Source(
          "source.lf",
          `
          type a = number
          type a = number
        `
        )
    );
    expect(errors.length).toBe(1);
    expect(errors[0].message).toEqual("Duplicate type 'a', first defined in source.lf:2");
  });
});

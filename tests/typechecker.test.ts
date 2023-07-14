import { MemorySourceLoader, Source } from "../lib";
import { compile } from "../lib/compiler";
import { ListType, MapType, NameAndType, NamedType, NothingType, NumberType, RecordType, StringType, UnionType } from "../lib/types";

describe("Typechecker tests", () => {
  it("Should infer type from initializer", () => {
    const { modules, errors } = compile(
      "source.lf",
      new MemorySourceLoader({
        path: "source.lf",
        text: `
          var a = nothing
          var b = true
          var c = 1.123
          var d = "Hello"
        `,
      })
    );
    expect(errors.length).toBe(0);
  });

  it("Should error if records of mixin share field names", () => {
    const { errors } = compile(
      "source.lf",
      new MemorySourceLoader({
        path: "source.lf",
        text: `
          type a = <x: number>
          type b = a + <y: number> + <x: string>
        `,
      })
    );
    expect(errors.length).toBe(1);
  });

  it("Should type check complex types", () => {
    const { modules, errors } = compile(
      "source.lf",
      new MemorySourceLoader({
        path: "source.lf",
        text: `
          type shapes = rectangle | circle | colored + <width: number>
          type color = <r: number, g: number, b: number>
          type colored = <color: color>
          type rectangle = colored + <width: number, height: number> # mixins!
          type circle = colored + <radius: number>
        `,
      })
    );

    const types = modules.get("source.lf")!.types;
    expect(errors.length).toBe(0);
    expect(types.has("shapes")).toBe(true);
    const shapes = types.get("shapes")! as NamedType;
    expect(shapes.type.kind).toEqual("union");
    const shapesType = shapes.type as UnionType;
    expect(shapesType.types.length).toBe(3);
    expect(shapesType.types[0]).toEqual(types.get("rectangle"));
    expect(shapesType.types[1]).toEqual(types.get("circle"));
    expect(shapesType.types[2]).toStrictEqual(
      new RecordType([new NameAndType("color", types.get("color")!), new NameAndType("width", types.get("number")!)])
    );
    expect(types.has("color")).toBe(true);
    const color = types.get("color")! as NamedType;
    expect(color.type.kind == "record").toBe(true);
    const colorType = color.type as RecordType;
    expect(colorType).toStrictEqual(
      new RecordType([
        new NameAndType("r", types.get("number")!),
        new NameAndType("g", types.get("number")!),
        new NameAndType("b", types.get("number")!),
      ])
    );
    expect(types.has("colored")).toBe(true);
    const colored = types.get("colored") as NamedType;
    expect(colored.type.kind == "record").toBe(true);
    const coloredType = colored.type as RecordType;
    expect(coloredType.fields.length).toBe(1);
    expect(coloredType.fields[0].type == color).toBe(true);
    expect(types.has("rectangle")).toBe(true);
    const rectangle = types.get("rectangle")! as NamedType;
    expect(rectangle.type.kind).toBe("record");
    expect(rectangle.type).toStrictEqual(
      new RecordType([
        new NameAndType("color", types.get("color")!),
        new NameAndType("width", types.get("number")!),
        new NameAndType("height", types.get("number")!),
      ])
    );
    expect(types.has("circle")).toBe(true);
    const circle = types.get("circle")! as NamedType;
    expect(circle.type.kind).toBe("record");
    expect(circle.type).toStrictEqual(
      new RecordType([new NameAndType("color", types.get("color")!), new NameAndType("radius", types.get("number")!)])
    );
  });

  it("Should error if not all types of a mixin are records", () => {
    const { errors } = compile(
      "source.lf",
      new MemorySourceLoader({
        path: "source.lf",
        text: `
          type a = <x: number>
          type b = a + <y: number> + number
        `,
      })
    );
    expect(errors.length).toBe(1);
    expect(errors[0].message).toEqual("All types in a mixin must be a record, but found 'number'.");
  });

  it("Should error on circular types", () => {
    const { errors } = compile(
      "source.lf",
      new MemorySourceLoader({
        path: "source.lf",
        text: `
          type a = number | b
          type b = a | number
        `,
      })
    );

    expect(errors.length).toBe(1);
    expect(errors[0].message).toEqual("Type 'b' circularly references itself.");
  });

  it("Should validate simple named types", () => {
    const { modules, errors } = compile(
      "source.lf",
      new MemorySourceLoader({
        path: "source.lf",
        text: `
          type a = nothing
          type b = number
          type c = [number]
          type d = {number}
          type e = <x: number, y: string>
          type u = a | b | nothing
        `,
      })
    );
    expect(errors.length).toBe(0);
    const types = modules.get("source.lf")!.types;

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
      new MemorySourceLoader({
        path: "source.lf",
        text: `
          type string = number
        `,
      })
    );
    expect(errors.length).toBe(1);
    expect(errors[0].message).toEqual("Can not use 'string' as a type name, as a built-in type with that name exists.");
  });

  it("Shouldn't allow duplicate named types", () => {
    const { errors } = compile(
      "source.lf",
      new MemorySourceLoader({
        path: "source.lf",
        text: `
          type a = number
          type a = number
        `,
      })
    );
    expect(errors.length).toBe(1);
    expect(errors[0].message).toEqual("Duplicate type 'a', first defined in source.lf:2.");
  });
});

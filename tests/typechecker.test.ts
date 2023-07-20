import { MemorySourceLoader, compile } from "../lib";
import { ListType, MapType, NameAndType, NamedType, NothingType, NumberType, RecordType, StringType, UnionType } from "../lib/types";
import { testCompile } from "./utils";

describe("Typechecker tests", () => {
  it("Should resolve imports.", () => {
    const { errors, modules } = compile(
      "source.lf",
      new MemorySourceLoader(
        {
          path: "source.lf",
          text: `
          import "c/d"
          var b = foo()
          b.x = 3
        `,
        },
        {
          path: "a/b.lf",
          text: `
          import "../c/d"
          export var b = foo()
        `,
        },
        {
          path: "c/d.lf",
          text: `
          type x = <x: number>
          export func foo()
            return x(123)
          end
        `,
        }
      )
    );
    expect(errors.length).toBe(0);
  });

  it("Should use stdlib operators, functions, types, and consts", () => {});

  it("Should perform more structural typing.", () => {
    const { errors } = testCompile(`
      type color = <r: number, g: number, b: number>
      type colored = <color: color>
      type car = colored + <kind: string>
      type bike = colored + <kind: string, foldable: boolean>

      var brompton = bike(color(255, 0, 0), "brompton", true)
      var beetle = car(color(255, 0, 255), "beetle")
      var things = [beetle, brompton]

      printColor(things)
      foo(things)

      func foo(v: [colored])
      end

      func printColor(coloredThing: colored | [colored])
        if coloredThing is colored then
          # print the thing
        elseif coloredThing is [colored] then
          for each thing in coloredThing do
            # print the thing
          end
        end
      end
    `);
  });

  it("Should perform structural typing.", () => {
    const { errors } = testCompile(`
      func bar(v: [<x: number>])
      end

      bar([<x: 0, y: 0>])

      func foo(v: [[<x: number>]] | [<x: number>])
      end

      foo([[<x: 0, y: 0, z: 0>]])
    `);
    expect(errors.length).toBe(0);
  });
  it("Should select the correct function based on argument types", () => {
    const { errors } = testCompile(`
      type vector = <x: number, y: number>

      func add(a: vector, b: vector): vector
        return <x: a.x + b.x, y: a.y + b.y>
      end

      func mul(v: vector, scalar: number): vector
        return <x: v.x * scalar, y: v.y * scalar>
      end

      var a: vector = <x: 0, y: 0>
      var b: vector = <x: 0, y: 0>
      var c = a.add(b).mul(2)

      func sumX(xs: [<x: number>])
        var sum = 0
        for each x in xs do
          sum = sum + x.x
        end
        for i from 0 to xs.length step 1 do
          sum = sum + xs[i].x
        end
        return sum
      end

      sumX([a, b, c])

      var list = [
        func(a: number) return a + 1 end,
        func(a: number) return a + 2 end,
        func(a: number) return a + 3 end
      ]
      list[0](0)

      var map = {
        "a": func(a: number) return a + 1 end,
        "b": func(a: number) return a + 2 end
      }
      map["a"](0)

      type foo = number | string
      func bar(a: foo)
      end
      bar(0)
      bar("string")

      func f(a: number)
        return a + 10
      end

      func f(a: number | string)
        if a is number then
          return a + 10
        end
      end

      func f(a: number | string | [number])
      end

      f([])

      var d = [0]
      f(d)

      var g = func(a: number)
        return a + 10
      end

      var n: number | string = "string"

      if n is number then
        g(n)
      end

      f(n)
      f([0])
    `);
    expect(errors.length).toBe(0);
  });

  it("Should infer types for empty list and map literals and expand literal types to unions", () => {
    const { errors } = testCompile(`
      var c:[[number] | number | [string]] = []
      c = [ ]
      var d:[[number]] = [[]]
      var e:[[number]] = [[], [], [0]]
      var f:[[number] | number | [string]] = []
      var g:[[number]|[string]] = [[:string], [0]]
      var i:[[number]|[string]] = []
      i = [[0], ["string"]]
      var x: number | string = 0
      var h: [number|string] = [0]
      var m: {number} = {}
      m = {}
      var n:{{number}} = {"a": {}}
      var o:{{number}} = {"a": {}, "b": {}, "c": { "d": 0}}
      var p:{{number} | number | {string}} = {}
      var q:{{number}|{string}} = {"a": {:string}, "b": {"c": 0}}
      q = {"a": {:number}, "b": {:number}}
      var y: {number|string} = {"a": 0}

      var r: <m: [number], r: <f: [string]>> = <m: [], r: <f: []>>
      var s: <x: number | string> = <x: 0>
      var xx: <x: number | string> | number = <x: 0>

      var z: [[[number|string]]] = [[[0, 1]], [[], ["string"]]]

      func foo(v: [[<x: number>]] | [<x: number>])
      end

      foo([[<x: 0, y: 0, z: 0>]])
    `);
    expect(errors.length).toBe(0);
  });

  it("Should fail recursive functions without a return type", () => {
    const { modules, errors } = testCompile(`
      func fibonacci(n: number)
        if (n <= 0) then return 0 end
        if (n == 1) then return 1 end
        return fibonacci(n - 1) + fibonacci(n - 2);
      end
    `);
    expect(errors.length).toBe(1);
    expect(errors[0].message).toEqual("Functions that are called recursively, either directly or indirectly, must have a return type.");
  });

  it("Should handle recursive functions", () => {
    const { modules, errors } = testCompile(`
      func fibonacci(n: number): number
        if (n <= 0) then return 0 end
        if (n == 1) then return 1 end
        return fibonacci(n - 1) + fibonacci(n - 2);
      end
    `);
    expect(errors.length).toBe(0);
  });

  it("Should update named function type when parameter and return type are resolved.", () => {
    const { modules, errors } = testCompile(`
      type x = <x: number>
      export func foo()
        return x(123)
      end
    `);
    expect(errors.length).toBe(0);
  });

  it("Should infer type from initializer", () => {
    const { modules, errors } = testCompile(`
      var a = nothing
      var b = true
      var c = 1.123
      var d = "Hello"
    `);
    expect(errors.length).toBe(0);
  });

  it("Should error if records of mixin share field names", () => {
    const { errors } = testCompile(`
      type a = <x: number>
      type b = a + <y: number> + <x: string>
    `);
    expect(errors.length).toBe(1);
  });

  it("Should type check complex types", () => {
    const { modules, errors } = testCompile(`
      type shapes = rectangle | circle | colored + <width: number>
      type color = <r: number, g: number, b: number>
      type colored = <color: color>
      type rectangle = colored + <width: number, height: number> # mixins!
      type circle = colored + <radius: number>
    `);

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
    const { errors } = testCompile(`
      type a = <x: number>
      type b = a + <y: number> + number
    `);
    expect(errors.length).toBe(1);
    expect(errors[0].message).toEqual("All types in a mixin must be a record, but found 'number'.");
  });

  it("Should not error on self referential types", () => {
    const { errors } = testCompile(`
      type node = <children: [node], value: number>
    `);

    expect(errors.length).toBe(0);
  });

  it("Should error on circular types", () => {
    const { errors } = testCompile(`
      type a = number | b
      type b = a | number
    `);

    expect(errors.length).toBe(1);
    expect(errors[0].message).toEqual("Type 'b' circularly references itself.");
  });

  it("Should validate simple named types", () => {
    const { modules, errors } = testCompile(`
      type a = nothing
      type b = number
      type c = [number]
      type d = {number}
      type e = <x: number, y: string>
      type u = a | b | nothing
    `);
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
    const { errors } = testCompile(`
      type string = number
    `);
    expect(errors.length).toBe(1);
    expect(errors[0].message).toEqual("Can not use 'string' as a type name, as a built-in type with that name exists.");
  });

  it("Shouldn't allow duplicate named types", () => {
    const { errors } = testCompile(`
      type a = number
      type a = number
    `);
    expect(errors.length).toBe(1);
    expect(errors[0].message).toEqual("Duplicate type 'a', first defined in source.lf:2.");
  });
});

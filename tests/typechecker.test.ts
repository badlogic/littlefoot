import { MemorySourceLoader, compile } from "../lib";
import { ListType, MapType, NameAndType, NamedType, NothingType, NumberType, RecordType, StringType, UnionType } from "../lib/types";
import { testCompile } from "./utils";

describe("Typechecker tests", () => {
  it("Should handle is operator correctly.", () => {
    const { errors, modules } = testCompile(`
    type t = number | string | nothing

    var x: t = 0

    if not x is string then
      var y = x
      if y is number then
        print(y)
      else
        print("Got nothing")
      end
    else
      print(x)
    end
    `);
    expect(errors.length).toBe(0);
  });
  it("Should check generics.", () => {
    const { errors, modules } = testCompile(`
    type r[T, R] = <f: (p: T): R>
    var rr: r[number, string | number] = <f: func(p: number)
      if p > 0 then
        return "test"
      else
        return 0
      end
    end
    >

    r(func (p: number): number return 0 end)

    type s[T] = <a: T, b: [T]>
    var ss: s[number] = <a: 0, b: [0]>
    var sc = <a: 0, b: [0]> as s[number]

    func add[T](a: T, b: T): T
      return a + b
    end

    add(1, 2)

    func foo[T](v: T): nothing
      if v is [T] then
      end
    end

    func forEach[V](list: [V], f: (element: V, index: number): nothing): nothing
      for index from 0 to list.length() do
        f(list[index], index)
      end
    end

    func filter[T](list: [T], f: (element: T, index: number): boolean): [T]
      const result: [T] = []
      for index from 0 to list.length() do
        if (f(list[index], index)) then
          push(result, list[index])
        end
      end
      return result
    end

    func map[I, O](list: [I], f: (element: I, index: number): O): [O]
      const result: [O] = []
      for index from 0 to list.length() do
        push(result, f(list[index], index))
      end
      return result
    end

    var numbers = [0, 1, 2, 3, 4, 5];
    numbers
      .map(func(element: number, index: number) return element + 1 end)
      .filter(func(element: number, index: number) return element >= 3 end)
      .forEach(func(n: number, index: number) print(n) end);

    map(numbers, func(element: number, index: number) return element + 1 end)
    filter(numbers, func(element: number, index: number) return element >= 3 end)
    forEach(numbers, func(n: number, index: number) print(n) end);

    type node[L, R] = <children: [node[L, R]], lValue: L, rValue: R>
    type root[V] = <left: node[V, V], right: node[V, V]>
    var left = node([:node[number, number]], 1, 1)
    var right = node([:node[number, number]], 2, 1)
    var r = root(left, right)

    type a[T] = <x: T>
    type b[T] = <v: a[T]>
    type c[T] = b[T]

    func bb[T](v: T): b[T] return b(a(0)) end

    var aa = a(10)
    var bb = b(aa)

    type opt[L] = <value: L | nothing>
    var a = 1;
    a.opt()

    func fuu[T](p: [(p: T | nothing): nothing], v: T): nothing
      for i from 0 to p.length() do
        p[i](v)
      end
      for each f in p do
        f(v)
      end
    end

    const funcs = [
      func(p: number | nothing): nothing end
    ]
    fuu(funcs, 0)

    type option[T] = T | nothing

    func fooz(value: option[number])
      if value is number then
        print(value)
      else
        print("Got nothing")
      end
    end

    fooz(nothing)

    type li[V] = [V]
    func barz[T](value: li[T]): nothing
    end

    barz([:number])
    `);
    expect(errors.length).toBe(0);
  });

  it("Should resolve imports.", () => {
    const { errors, modules } = compile(
      "source.lf",
      new MemorySourceLoader(
        {
          path: "source.lf",
          text: `
          import "a/b"
          import x, foo, d as e from "c/d"
          var c = foo()
          var t: x = x(12)
          e.x = c.x
          var z = x(10)
        `,
        },
        {
          path: "a/b.lf",
          text: `
          import "../c/d"
          export const b = foo()
        `,
        },
        {
          path: "c/d.lf",
          text: `
          type x = <x: number>
          type u = number | string
          export func foo()
            return x(123)
          end
          export var d = foo()
        `,
        }
      )
    );
    expect(errors.length).toBe(0);
  });

  it("Should use stdlib operators, functions, types, and consts", () => {});

  it("Should check duplicate generic functions", () => {
    const { errors } = testCompile(`
    func foo(a: number)
    end

    type a[T] = <x: T>

    func foo[Z](a: Z, b: [Z], c: {Z}, d: a[Z], e: a[Z]): nothing
    end

    func foo[V](a: V, b: [V], c: {V}, d: a[V], e: a[V]): nothing
    end`);
    expect(errors.length).toBe(1);
  });

  it("Should perform more structural typing.", () => {
    const { errors } = testCompile(`
    type color = <r: number, g: number, b: number>
    type colored = <color: color>
    type car = colored + <kind: string>
    type bike = colored + <kind: string, foldable: boolean>

    var brompton = bike(color(255, 0, 0), "brompton", true)
    var beetle = car(color(255, 0, 255), "beetle")

    func toString(colored: colored): string
      return
        colored.color.r.toString() + ", " +
        colored.color.g.toString() + ", " +
        colored.color.b.toString()
    end

    func printColor(coloredThing: colored)
      print(coloredThing.toString())
    end
    printColor(beetle)

    func foo(things: [colored] | [colored | number])
      if things is [colored] then
        for each thing in things do
          thing.toString()
        end
      elseif things is [colored | number] then
        for each thing in things do
          if thing is colored then
            thing.toString()
          end
        end
      end
    end
    const cars = [beetle, beetle];
    foo(cars)
    foo([beetle, beetle] as [car])
    foo([beetle, brompton])
    `);
    expect(errors.length).toBe(0);
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
        for i from 0 to xs.length() step 1 do
          sum = sum + xs[i].x
        end
        return sum
      end

      sumX([a, b, c])

      var list = [
        func(a: number) return a + 1 end,
        func(a: string) return a + "test" end,
        func(a: number) return a + 3 end
      ]
      var fg = list[0]
      if fg is (a: number): number then
        fg(0)
      end

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
      type s[T] = <a: T, b: [T]>

      func s[T](a: T): s[T]
        return s(a, [a])
      end

      var ss = s(0)
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
      var mm: {number} = {}
      mm = {}
      var n:{{number}} = {"a": {}}
      var o:{{number}} = {"a": {}, "b": {}, "c": { "d": 0}}
      var p:{{number} | number | {string}} = {}
      var q:{{number}|{string}} = {"a": {:string}, "b": {"c": 0}}
      q = {"a": {:number}, "b": {:number}}
      var y: {number|string} = {"a": 0}

      var rr: <m: [number], r: <f: [string]>> = <m: [], r: <f: []>>
      var s: <x: number | string> = <x: 0>
      var xx: <x: number | string> | number = <x: 0>

      var z: [[[number|string]]] = [[[0, 1]], [[], ["string"]]]

      func foo(v: [[<x: number>]] | [<x: number>])
      end

      foo([[<x: 0, y: 0, z: 0>]])

      var l1: [number | string] = [0, 1, 2]
      var l2: [number | string] | [number] = [0, 1, 2]
      var l3: [[number | string] | [number]] = [[0, 1, 2]]
      var l4: [[number | string] | [number]] = [[0, 1, 2], ["string"]]
      func l(p: [[number | string] | [number]])
      end
      l([[0, 1, 2]])

      var m1: {number | string} = {"a": 0}
      var m2: {number | string} | {number} = {"a": 0}
      var m3: {{number | string} | {number}} = {"a" : {"a": 0}}
      var m4: {{number | string} | {number}} = {"a" : {"a": 0}, "b": { "b" : "string"}}

      func m(p: {{number | string} | {number}})
      end
      m({"a" : {"a": 0}})

      var r1: <x: number | string> = <x: 0>
      var r2: <x: number | string> | <x: number> = <x: 0>
      var r3: <x: <y: number | string> | <y: number>> = <x: <y: 0>>
      func r(p: <x: <y: number | string> | <y: number>>)
      end
      r(<x: <y: 0>>)

      func nn(): number
        return 0
      end
      var rt = nn()
      var nn: number | string = rt
      nn = 0

      type node = <children: [node], value: number | string | nothing>
      var tree1: node = <children: [], value: 0>
      var tree2: node = <children: [
        <children: [], value: nothing>,
        <children: [], value: nothing>,
        <children: [], value: 123>
      ], value: 1>

      var tree: node = node([
        node([
          node([], nothing)
        ], "test")
      ], 1)
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

  it("Should handle recursive generic type defintions", () => {
    const { modules, errors } = testCompile(`
    type branch[T] = <left: node[T], right: node[T]>
    type leaf[T] = T
    type node[T] = branch[T] | leaf[T]
    var left = branch(0, 0)
    var root = branch(left, left)

    func traverse[T](node: node[T], level: number): nothing
      for i from 0 to level do print("  ") end
      if node is leaf[T] then
        print(node)
      else
        traverse(node.left, level + 1)
        traverse(node.right, level + 1)
      end
    end
    traverse(root, 0)
    `);
    expect(errors.length).toBe(0);
  });

  it("Should handle recursive type definitions", () => {
    const { modules, errors } = testCompile(`
    type branch = <left: node, right: node>
    type leaf = number
    type node = branch | leaf
    var root = branch(0, branch(0, branch(0, 1)))

    func traverse(node: node, level: number): nothing
      for i from 0 to level do print("  ") end
      if node is leaf then
        print(node)
      else
        traverse(node.left, level + 1)
        traverse(node.right, level + 1)
      end
    end
    `);
    expect(errors.length).toBe(0);
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

      var c: a = 0 as b
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

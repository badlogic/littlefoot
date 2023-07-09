import * as fs from "fs";
import { Source, parse } from "../lib";
import { checkTypes } from "../lib/typechecker";
import { NamedType, NothingType, NumberType, Types, UnionType } from "../lib/types";

describe("Typechecker tests", () => {
  it("Should validate simple named types", () => {
    const { ast, errors } = parse(
      new Source(
        "source.lf",
        `
        type a = nothing
        type b = number
        type u = a | b
    `
      )
    );
    const types = new Types();
    checkTypes(ast, errors, types);

    const a = types.get("a")! as NamedType;
    expect(a.kind == "named type").toBe(true);
    expect(a.type == NothingType).toBe(true);

    const b = types.get("b")! as NamedType;
    expect(b.kind == "named type").toBe(true);
    expect(b.type == NumberType).toBe(true);

    const u = types.get("u")! as NamedType;
    expect(u.kind == "named type").toBe(true);
    expect(u.type.kind == "union").toBe(true);
    expect((u.type as UnionType).types[0] === a).toBe(true);
    expect((u.type as UnionType).types[1] === b).toBe(true);
  });

  it("Shouldn't allow built-in names for named types", () => {
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

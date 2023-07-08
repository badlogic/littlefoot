import { expect } from "chai";
import * as fs from "fs";
import { Source, parse } from "../lib";

describe("Parser tests", () => {
  it("Should parse the parser example file", () => {
    const file = fs.readFileSync("tests/example.lf", { encoding: "utf-8" });
    const { ast, errors } = parse(new Source("source.lf", file));
    expect(errors.length).to.be.equal(0);
  });
});

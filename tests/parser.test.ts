import * as fs from "fs";
import { Source, parse } from "../lib";
import { CompilerContext } from "../lib/compiler";
import { traverseAst } from "../lib/ast";

describe("Parser tests", () => {
  it("Should parse the parser example file", () => {
    const file = fs.readFileSync("tests/example.lf", { encoding: "utf-8" });
    const context = new CompilerContext(() => null);
    const ast = parse(new Source("source.lf", file), context.errors);
    expect(context.errors.length).toBe(0);
    for (const node of ast) {
      traverseAst(node, (node) => {
        return true;
      });
    }
  });
});

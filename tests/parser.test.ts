import { parse } from "../lib";
import { traverseAst } from "../lib/ast";
import { CompilerContext } from "../lib/compiler";
import { FileSourceLoader } from "./utils";

describe("Parser tests", () => {
  it("Should parse the parser example file", () => {
    const context = new CompilerContext(new FileSourceLoader("./"));
    const ast = parse(context.sourceLoader.load("tests/parser-test.lf")!, context.errors);
    expect(context.errors.length).toBe(0);
    for (const node of ast) {
      traverseAst(node, (node) => {
        return true;
      });
    }
  });
});

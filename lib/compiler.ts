import { AstNode, StatementNode } from "./ast";
import { LittleFootError } from "./error";
import { parse } from "./parser";
import { Source, SourceLoader, SourceLocation } from "./source";
import { checkTypes } from "./typechecker";
import { FunctionType, NamedFunction, NothingType, Types } from "./types";

export class Module {
  constructor(public readonly path: string, public readonly ast: AstNode[]) {}
}

export class CompilerContext {
  public readonly errors: LittleFootError[] = [];
  public readonly types = new Types();
  public readonly sources = new Map<String, Source>();
  public readonly modules = new Map<String, Module>();

  constructor(public readonly sourceLoader: SourceLoader) {}

  getSource(path: string): Source | null {
    if (this.sources.has(path)) return this.sources.get(path)!;
    const source = this.sourceLoader.load(path);
    if (!source) return null;
    this.sources.set(path, source);
    return source;
  }
}

export function compile(path: string, sourceLoader: SourceLoader) {
  const context = new CompilerContext(sourceLoader);
  const source = context.getSource(path);
  if (!source) throw new Error(`Couldn't find source with path '${path}'`);

  const ast = parse(source, context.errors);

  // Extract all top level statements into a generated $main function.
  const mainStatements = ast.filter((node) => {
    return node.kind != "import" && node.kind != "function declaration" && node.kind != "type declaration";
  });
  context.types.add(
    new NamedFunction(
      "$moduleInit",
      new FunctionType([], NothingType),
      mainStatements,
      false,
      false,
      new SourceLocation(source, 0, source.text.length)
    )
  );

  checkTypes(ast, context);
  context.modules.set(path, new Module(path, ast));
  return context;
}

import { AstNode, StatementNode } from "./ast";
import { LittleFootError } from "./error";
import { parse } from "./parser";
import { Source } from "./source";
import { tokenize } from "./tokenizer";
import { checkTypes } from "./typechecker";
import { Types } from "./types";

export class Module {
  constructor(public readonly path: string, public readonly ast: AstNode[]) {}
}

export class CompilerContext {
  public readonly errors: LittleFootError[] = [];
  public readonly types = new Types();
  public readonly sources = new Map<String, Source>();
  public readonly modules = new Map<String, Module>();

  constructor(public readonly loadSource: (path: string) => Source | null) {}

  getSource(path: string): Source | null {
    if (this.sources.has(path)) return this.sources.get(path)!;
    const source = this.loadSource(path);
    if (!source) return null;
    this.sources.set(path, source);
    return source;
  }
}

export function compile(path: string, loadSource: (path: string) => Source | null) {
  const context = new CompilerContext(loadSource);
  const source = context.getSource(path);
  if (!source) throw new Error(`Couldn't find source with path '${path}'`);
  let ast = parse(source, context.errors);
  // Extract all top level statements into a generated $main function.
  let mainStatements = ast.filter((node) => (node as StatementNode).kind !== undefined);
  checkTypes(ast, context);
  context.modules.set(path, new Module(path, ast));
  return context;
}

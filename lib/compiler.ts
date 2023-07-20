import { AstNode, FunctionLiteralNode, StatementNode, VariableNode } from "./ast";
import { LittleFootError } from "./error";
import { parse } from "./parser";
import { Source, SourceLoader, SourceLocation } from "./source";
import { IdentifierToken } from "./tokenizer";
import { TypeCheckerContext, checkTypes } from "./typechecker";
import { FunctionType, Functions, NamedFunctionType, NothingType, Types } from "./types";

export class Module {
  constructor(
    public readonly path: string,
    public readonly types = new Types(),
    public readonly functions = new Functions(),
    public readonly variables = new Map<string, VariableNode>(),
    public ast: AstNode[] = []
  ) {}
}

export class CompilerContext {
  public readonly errors: LittleFootError[] = [];
  public readonly sources = new Map<string, Source>();
  public readonly modules = new Map<string, Module>();
  public readonly moduleStack = new Array<Module>();

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
  compileModule(path, context);
  return context;
}

function resolvePath(path: string): string {
  const parts = path.split("/");
  const resolvedPath = [];

  for (const part of parts) {
    if (part === "..") {
      resolvedPath.pop();
    } else {
      resolvedPath.push(part);
    }
  }

  return resolvedPath.join("/");
}

function getAbsolutePath(parentDir: string, path: string): string {
  const parentDirWithoutFile = parentDir.split("/").slice(0, -1).join("/");
  const resolvedParentDir = resolvePath(parentDirWithoutFile);
  const combinedPath = resolvedParentDir.length == 0 ? path : `${resolvedParentDir}/${path}`;
  const absolutePath = resolvePath(combinedPath);
  return absolutePath;
}

export function compileModule(path: string, context: CompilerContext) {
  const parentDir = context.moduleStack.length > 0 ? context.moduleStack[context.moduleStack.length - 1].path : "";
  const absolutePath = getAbsolutePath(parentDir, path) + (path.endsWith(".lf") ? "" : ".lf");
  if (context.modules.has(absolutePath)) return context.modules.get(absolutePath)!;
  const source = context.getSource(absolutePath);
  if (!source) throw new Error(`Couldn't find source with path '${absolutePath}'`);

  const module = new Module(absolutePath);
  context.moduleStack.push(module);
  context.modules.set(absolutePath, module);

  module.ast = parse(source, context.errors);

  // Extract all top level statements into a generated $main function.
  const mainStatements = module.ast.filter((node) => {
    return node.kind != "import" && node.kind != "function declaration" && node.kind != "type declaration";
  }) as StatementNode[];
  const mainLocation = new SourceLocation(source, 0, source.text.length);
  const mainNode = new FunctionLiteralNode(new IdentifierToken(mainLocation, source.text), [], null, mainStatements, mainLocation);
  module.functions.add("$main", new NamedFunctionType("$main", new FunctionType([], NothingType), mainNode, false, false, mainLocation));

  checkTypes(new TypeCheckerContext(module, context));

  // Gather all module level vars
  module.ast.forEach((node) => {
    if (node.kind == "variable declaration") {
      module.variables.set(node.name.value, node);
    }
  });

  context.moduleStack.pop();
  return module;
}

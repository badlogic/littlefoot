import { LittleFootError, MemorySourceLoader, SourceLocation, Token } from "../lib";
import { Module, compile } from "../lib/compiler";
import { BaseType, NamedType, seenTypes, setGenerateTypeIds } from "../lib/types";
// @ts-ignore
import example from "../tests/example.lf";
import { Editor } from "./editor";
import { AstNode, BaseAstNode, traverseAst } from "../lib/ast";
import { Visualizer } from "./visualizer";
import { LittlefootCompletionsProvider } from "./completions";

setGenerateTypeIds(true);
const editorContainer = document.querySelector("#editor") as HTMLTextAreaElement;
const errorDiv = document.querySelector("#errors") as HTMLDivElement;
const outputDiv = document.querySelector("#output") as HTMLDivElement;
const modulesDiv = document.querySelector("#modules") as HTMLDivElement;
let outputApp: any = null;
let errors: LittleFootError[] = [];
let modules = new Map<string, Module>();
let completions = new LittlefootCompletionsProvider();

const editor = new Editor(
  editorContainer,
  (newText: string) => {
    localStorage.setItem("source", editor.value);
    compileText(editor.value);
  },
  (start, end) => {
    highlightSelection(start, end);
  }
);
editor.setCompletionProvider(completions);

const stored = localStorage.getItem("source");
if (stored) editor.value = stored;
else {
  editor.value = example;
}

function compileText(value: string) {
  localStorage.setItem("source", value);
  let start = performance.now();
  const context = compile("source.lf", new MemorySourceLoader({ path: "source.lf", text: value }));
  modules = context.modules;
  errors = context.errors;
  console.log(`Compilation took: ${(performance.now() - start).toFixed(2)} ms`);
  completions.modules = modules;
  showModules(modules);
  showErrors(errors);
  showOutput("Module source.lf", modules.get("source.lf")?.ast);
}

function showErrors(errors: LittleFootError[]) {
  if (errors.length > 0) {
    editor.highlightErrors(errors);
    const html = errors
      .map((error) => error.toStringWithCauses() + "\n")
      .reduce((prev, curr) => prev + curr)
      .replace(/</g, "&lt;")
      .replace(/\n/g, "<br>");
    errorDiv.innerHTML = html;
  } else {
    errorDiv.innerHTML = "0 Errors";
    editor.highlightErrors([]);
  }
}

let timeoutId: any = 0;
function showOutput(rootName: string, thing: any) {
  clearTimeout(timeoutId);
  timeoutId = setTimeout(() => {
    new Visualizer(
      outputDiv,
      thing,
      rootName,
      (path, key, value) => {
        if (path.length > 1) return false;
        return true;
      },
      (object) => {
        const keys = Object.keys(object).filter((key) => {
          if (key == "kind" || key == "type" || key == "name") return false;
          return true;
        });
        if (object["type"]) keys.unshift("type");
        if (object["name"]) keys.unshift("name");
        if (object["kind"]) keys.unshift("kind");
        return keys;
      },
      (obj, key, value) => {
        return value;
      },
      (value) => {
        if (value instanceof SourceLocation) return value.toString();
        if (value instanceof BaseType) return value.signature;
        if (value instanceof Token) return JSON.stringify(value.value);
        if (value instanceof BaseAstNode) return (value as any)["kind"] + ", type: " + value.type.signature;
        return undefined;
      }
    );
  }, 200);
}

function newTreeNode(level: number, text: string, callback: (() => void) | null = null) {
  const div = document.createElement("div");
  div.innerText = text;
  if (callback) {
    div.addEventListener("click", callback);
  }
  div.classList.add("treeNode");
  div.classList.add("treeNode" + level);
  div.style.paddingLeft = level + "em";
  return div;
}

function newSpacer() {
  const div = document.createElement("div");
  div.style.height = "1em";
  div.style.width = "100%";
  return div;
}

function showModules(modules: Map<string, Module>) {
  modulesDiv.innerHTML = "";
  modules.forEach((module, name) => {
    if (name == "stdlib.lf") return;
    modulesDiv.appendChild(newTreeNode(1, "Module " + name));
    modulesDiv.appendChild(newTreeNode(2, "AST", () => showOutput("Module " + name + " AST", module.ast)));

    modulesDiv.appendChild(newTreeNode(2, "Types"));
    module.types.lookup.forEach((type, name) => {
      if (type.kind == "primitive") return;
      if (type.location.source.path != module.source.path) return;
      if (type.isInstantiated) return;
      modulesDiv.appendChild(newTreeNode(3, `${name}: ${type.signature}`, () => showOutput("Type " + type.signature, type)));
    });

    modulesDiv.appendChild(newTreeNode(2, "Instantiated Types"));
    module.types.lookup.forEach((type, name) => {
      if (type.kind == "primitive") return;
      if (type.location.source.path != module.source.path && !type.isInstantiated) return;
      if (!type.isInstantiated) return;
      modulesDiv.appendChild(newTreeNode(3, `${name}: ${type.signature}`, () => showOutput("Type " + type.signature, type)));
    });

    modulesDiv.appendChild(newTreeNode(2, "Functions"));
    module.functions.lookup.forEach((funcs) => {
      const moduleFuncs = funcs.filter(
        (func) => func.location.source.path == module.source.path && ((func.genericTypes.length == 0 && func.isInstantiated) || !func.isInstantiated)
      );
      if (moduleFuncs.length > 0) {
        moduleFuncs.forEach((func) =>
          modulesDiv.appendChild(
            newTreeNode(3, func.signature, () => {
              // checkNodeTypes(func.ast, new TypeCheckerContext(module, new CompilerContext(new MemorySourceLoader())));
              showOutput("Function " + func.signature, func);
            })
          )
        );
      }
    });

    modulesDiv.appendChild(newTreeNode(2, "Instantiated Functions"));
    module.functions.lookup.forEach((funcs) => {
      const moduleFuncs = funcs.filter((func) => func.isInstantiated && func.genericTypes.length > 0);
      if (moduleFuncs.length > 0) {
        moduleFuncs.forEach((func) => {
          // checkNodeTypes(func.ast, new TypeCheckerContext(module, new CompilerContext(new MemorySourceLoader())));
          modulesDiv.appendChild(newTreeNode(3, func.signature, () => showOutput("Function " + func.signature, func)));
        });
      }
    });
  });
  modulesDiv.appendChild(newSpacer());
  modulesDiv.appendChild(newTreeNode(1, "All types"));
  seenTypes.forEach((type) => {
    modulesDiv.appendChild(newTreeNode(3, `${type.id}: ${type.signature}`, () => showOutput("Type " + type.signature, type)));
  });
}

function highlightSelection(start: number, end: number) {
  const module = modules.get("source.lf");
  if (!module) return;

  if (start == end) {
    showOutput("Module source.lf", module.ast);
    return;
  }
  const inRange = (location: SourceLocation, start: number, end: number) => {
    return start >= location.start && end <= location.end;
  };
  let bestNode: AstNode | null = null;
  let bestMatchLength = Infinity;

  const callback = (node: AstNode, parentNode: AstNode | null): boolean => {
    if (!inRange(node.location, start, end)) return false;
    const nodeLength = node.location.end - node.location.start;
    const matchLength = Math.min(Math.abs(start - node.location.start), Math.abs(end - node.location.end));

    if (!bestNode) {
      bestNode = node;
      bestMatchLength = matchLength;
    } else {
      if (
        matchLength < bestMatchLength ||
        (matchLength === bestMatchLength && nodeLength < (bestNode?.location.end - bestNode?.location.start || Infinity))
      ) {
        bestNode = node;
        bestMatchLength = matchLength;
      }
    }
    return true;
  };

  for (const statement of module.ast) {
    traverseAst(statement, null, callback);
  }
  if (bestNode) {
    showOutput("AST node", bestNode);
  } else {
    showOutput("Module source.lf", module.ast);
  }
}

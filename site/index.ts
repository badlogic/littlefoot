import { LittleFootError, MemorySourceLoader, Source } from "../lib";
import { traverseAst } from "../lib/ast";
import { CompilerContext, Module, compile } from "../lib/compiler";
import { TypeCheckerContext, checkNodeTypes } from "../lib/typechecker";
// @ts-ignore
import example from "../tests/example.lf";
import { Editor } from "./editor";

const editorContainer = document.querySelector("#editor") as HTMLTextAreaElement;
const errorDiv = document.querySelector("#errors") as HTMLDivElement;
const outputDiv = document.querySelector("#output") as HTMLDivElement;
const modulesDiv = document.querySelector("#modules") as HTMLDivElement;

const editor = new Editor(
  editorContainer,
  (newText: string) => {
    localStorage.setItem("source", editor.value);
    compileText(editor.value);
  },
  (start, end) => {}
);

const stored = localStorage.getItem("source");
if (stored) editor.value = stored;
else {
  editor.value = example;
}

function compileText(value: string) {
  localStorage.setItem("source", value);
  let start = performance.now();
  const { errors, modules } = compile("source.lf", new MemorySourceLoader({ path: "source.lf", text: value }));
  console.log(`Compilation took: ${(performance.now() - start).toFixed(2)} ms`);

  showModule(modules);
  showErrors(errors);
  showOutput(modules.get("source.lf")?.ast);
}

function showErrors(errors: LittleFootError[]) {
  if (errors.length > 0) {
    editor.highlightErrors(errors);
    const html = errors
      .map((error) => error.toStringWithCauses())
      .reduce((prev, curr) => prev + curr)
      .replace(/</g, "&lt;")
      .replace(/\n/g, "<br>");
    errorDiv.innerHTML = html;
  } else {
    errorDiv.innerHTML = "0 Errors";
    editor.highlightErrors([]);
  }
}

function showOutput(thing: any) {
  outputDiv.innerHTML = "";
  outputDiv.innerHTML += JSON.stringify(
    thing,
    (key, value) => {
      if (key == "source" || key == "location" || key == "typeNode") return undefined;
      if (key == "type" && value.signature) return value.signature;
      if (key == "name" && value.value) return value.value;
      return value;
    },
    2
  )
    .replace(/</g, "&lt;")
    .replace(/\n/g, "<br>");
}

function newTreeNode(level: number, text: string, callback: (() => void) | null = null) {
  const div = document.createElement("div");
  div.innerText = text;
  if (callback) {
    div.addEventListener("click", callback);
    div.style.color = "#007bff";
  }
  div.classList.add("treeNode" + level);
  div.style.cursor = "pointer";
  div.style.paddingLeft = level + "em";
  return div;
}

function showModule(modules: Map<string, Module>) {
  modulesDiv.innerHTML = "";
  modules.forEach((module, name) => {
    if (name == "stdlib.lf") return;
    modulesDiv.appendChild(newTreeNode(1, "Module " + name));
    modulesDiv.appendChild(newTreeNode(2, "AST", () => showOutput(module.ast)));

    modulesDiv.appendChild(newTreeNode(2, "Types"));
    module.types.lookup.forEach((type, name) => {
      if (type.kind == "primitive") return;
      if (type.location.source.path != module.source.path) return;
      if (type.isInstantiated) return;
      modulesDiv.appendChild(newTreeNode(3, `${name}: ${type.signature}`, () => showOutput(type)));
    });

    modulesDiv.appendChild(newTreeNode(2, "Instantiated Types"));
    module.types.lookup.forEach((type, name) => {
      if (type.kind == "primitive") return;
      if (type.location.source.path != module.source.path && !type.isInstantiated) return;
      if (!type.isInstantiated) return;
      modulesDiv.appendChild(newTreeNode(3, `${name}: ${type.signature}`, () => showOutput(type)));
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
              showOutput(func);
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
          modulesDiv.appendChild(newTreeNode(3, func.signature, () => showOutput(func)));
        });
      }
    });
  });
}

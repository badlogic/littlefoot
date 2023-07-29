import { LittleFootError, MemorySourceLoader, Source } from "../lib";
import { traverseAst } from "../lib/ast";
import { Module, compile } from "../lib/compiler";
// @ts-ignore
import example from "../tests/example.lf";
import { Editor } from "./editor";

const editorContainer = document.querySelector("#editor") as HTMLTextAreaElement;
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
  const { errors, modules } = compile("source.lf", new MemorySourceLoader({ path: "source.lf", text: value }));

  showOutput(errors, modules);
  showModule(modules);
}

function showOutput(errors: LittleFootError[], modules: Map<string, Module>) {
  outputDiv.innerHTML = "";

  if (errors.length > 0) {
    editor.highlightErrors(errors);
    const html = errors
      .map((error) => error.toString())
      .reduce((prev, curr) => prev + curr)
      .replace(/</g, "&lt;")
      .replace(/\n/g, "<br>");
    outputDiv.innerHTML = html;
  } else {
    editor.highlightErrors([]);
  }

  const ast = modules.get("source.lf")!.ast;
  outputDiv.innerHTML += JSON.stringify(
    ast,
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

function showModule(modules: Map<string, Module>) {
  modulesDiv.innerHTML = "";
  modules.forEach((module, name) => {
    modulesDiv.innerHTML += "Module " + name + "<br>";
    modulesDiv.innerHTML += "   Types<br>";
    module.types.lookup.forEach((type, name) => {
      modulesDiv.innerHTML += `      ${name}: ${type.signature}<br>`;
    });

    modulesDiv.innerHTML += "   Functions<br>";
    module.functions.lookup.forEach((funcs, name) => {
      modulesDiv.innerHTML += "      " + name + "<br>";
      funcs.forEach((func) => {
        modulesDiv.innerHTML += `         ${func.signature}<br>`;
      });
    });
  });
}

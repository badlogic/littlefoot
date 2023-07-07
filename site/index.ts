import "monaco-editor/min/vs/editor/editor.main.css";
import * as monaco from "monaco-editor";
import { parse, Source } from "../lib";

const editorContainer = document.querySelector("#editor") as HTMLTextAreaElement;
const source = document.querySelector("#source") as HTMLTextAreaElement;
const output = document.querySelector("#output") as HTMLDivElement;

function tokenize(value: string) {
  localStorage.setItem("source", value);
  const sourceDoc = new Source("source", value);
  const { ast, errors } = parse(sourceDoc);
  if (errors.length == 0) {
    output.innerHTML = JSON.stringify(
      ast,
      (key, value) => (key == "source" || key == "firstToken" || key == "lastToken" ? undefined : value),
      2
    ).replace(/\n/g, "<br>");
  } else {
    output.innerHTML = errors
      .map((error) => error.toString())
      .reduce((prev, curr) => prev + curr)
      .replace(/\n/g, "<br>");
  }
}

(globalThis as any).self.MonacoEnvironment = {
  getWorkerUrl: function (moduleId: any, label: any) {
    return "./build/editor.worker.js";
  },
};

const editor = monaco.editor.create(editorContainer, {
  automaticLayout: true,
  theme: "vs-dark",
  value: "This is a test",
  minimap: {
    enabled: false,
  },
});

editor.onDidChangeModelContent(() => {
  localStorage.setItem("source", editor.getValue());
  tokenize(editor.getValue());
});

const stored = localStorage.getItem("source");
if (stored) editor.setValue(stored);
tokenize(editor.getValue());

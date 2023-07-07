import "monaco-editor/min/vs/editor/editor.main.css";
// @ts-ignore
import example from "../tests/parser-example.lf";
import * as monaco from "monaco-editor";
import { LittleFootError, parse, Source } from "../lib";

const editorContainer = document.querySelector("#editor") as HTMLTextAreaElement;
const source = document.querySelector("#source") as HTMLTextAreaElement;
const output = document.querySelector("#output") as HTMLDivElement;

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
else {
  editor.setValue(example);
}
tokenize(editor.getValue());

function tokenize(value: string) {
  localStorage.setItem("source", value);
  const sourceDoc = new Source("source", value);
  const { ast, errors } = parse(sourceDoc);
  if (errors.length == 0) {
    highlightErrors([]);
    output.innerHTML = JSON.stringify(
      ast,
      (key, value) => (key == "source" || key == "firstToken" || key == "lastToken" ? undefined : value),
      2
    ).replace(/\n/g, "<br>");
  } else {
    highlightErrors(errors);
    output.innerHTML = errors
      .map((error) => error.toString())
      .reduce((prev, curr) => prev + curr)
      .replace(/\n/g, "<br>");
  }
}

function highlightErrors(errors: LittleFootError[]) {
  const markers: monaco.editor.IMarkerData[] = [];
  errors.forEach((error) => {
    const startPos = editor.getModel()!.getPositionAt(error.start);
    const endPos = editor.getModel()!.getPositionAt(error.end);
    markers.push({
      severity: monaco.MarkerSeverity.Error,
      startLineNumber: startPos.lineNumber,
      startColumn: startPos.column,
      endLineNumber: endPos.lineNumber,
      endColumn: endPos.column,
      message: error.message,
    });
  });
  monaco.editor.setModelMarkers(editor.getModel()!, "littlefoot", markers);
}

// @ts-ignore
import example from "../tests/example.lf";
import { parse, Source } from "../lib";
import { Editor } from "./editor";
import { checkTypes } from "../lib/typechecker";
import { Types } from "../lib/types";

const editorContainer = document.querySelector("#editor") as HTMLTextAreaElement;
const output = document.querySelector("#output") as HTMLDivElement;

const editor = new Editor(editorContainer, (newText: string) => {
  localStorage.setItem("source", editor.value);
  compile(editor.value);
});

const stored = localStorage.getItem("source");
if (stored) editor.value = stored;
else {
  editor.value = example;
}
compile(editor.value);

function compile(value: string) {
  localStorage.setItem("source", value);
  const sourceDoc = new Source("source", value);
  const { ast, errors } = parse(sourceDoc);
  const types = new Types();
  checkTypes(ast, errors, types);

  if (errors.length == 0) {
    editor.highlightErrors([]);
    output.innerHTML = JSON.stringify(
      ast,
      (key, value) => (key == "source" || key == "firstToken" || key == "lastToken" ? undefined : value),
      2
    ).replace(/\n/g, "<br>");
  } else {
    editor.highlightErrors(errors);
    output.innerHTML = errors
      .map((error) => error.toString())
      .reduce((prev, curr) => prev + curr)
      .replace("<", "&lt;")
      .replace(/\n/g, "<br>");
  }
}

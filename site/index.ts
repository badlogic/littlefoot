// @ts-ignore
import example from "../tests/example.lf";
import { parse, Source } from "../lib";
import { Editor } from "./editor";

const editorContainer = document.querySelector("#editor") as HTMLTextAreaElement;
const output = document.querySelector("#output") as HTMLDivElement;

const editor = new Editor(editorContainer, (newText: string) => {
  localStorage.setItem("source", editor.value);
  tokenize(editor.value);
});

const stored = localStorage.getItem("source");
if (stored) editor.value = stored;
else {
  editor.value = example;
}
tokenize(editor.value);

function tokenize(value: string) {
  localStorage.setItem("source", value);
  const sourceDoc = new Source("source", value);
  const { ast, errors } = parse(sourceDoc);
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
      .replace(/\n/g, "<br>");
  }
}

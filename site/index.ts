import { MemorySourceLoader, Source } from "../lib";
import { traverseAst } from "../lib/ast";
import { compile } from "../lib/compiler";
// @ts-ignore
import example from "../tests/example.lf";
import { Editor } from "./editor";

const editorContainer = document.querySelector("#editor") as HTMLTextAreaElement;
const output = document.querySelector("#output") as HTMLDivElement;

const editor = new Editor(editorContainer, (newText: string) => {
  localStorage.setItem("source", editor.value);
  compileText(editor.value);
});

const stored = localStorage.getItem("source");
if (stored) editor.value = stored;
else {
  editor.value = example;
}
compileText(editor.value);

function compileText(value: string) {
  localStorage.setItem("source", value);
  const { errors, modules } = compile("source.lf", new MemorySourceLoader({ path: "source.lf", text: value }));

  if (errors.length == 0) {
    const ast = modules.get("source.lf")!.ast;
    for (const node of ast) {
      traverseAst(node, (n) => {
        n.type.resolvedSignature;
        return true;
      });
    }
    editor.highlightErrors([]);
    output.innerHTML = JSON.stringify(
      ast,
      (key, value) => (key == "source" || key == "firstToken" || key == "lastToken" || key == "typeNode" || key == "code" ? undefined : value),
      2
    )
      .replace(/</g, "&lt;")
      .replace(/\n/g, "<br>");
  } else {
    editor.highlightErrors(errors);
    const html = errors
      .map((error) => error.toString())
      .reduce((prev, curr) => prev + curr)
      .replace(/</g, "&lt;")
      .replace(/\n/g, "<br>");
    output.innerHTML = html;
  }
}

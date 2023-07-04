import { parse, Source } from "../lib";

const source = document.querySelector("#source");
const output = document.querySelector("#output");

function tokenize(value) {
  localStorage.setItem("source", value);
  const sourceDoc = new Source("source", value);
  const { ast, errors } = parse(sourceDoc);
  if (errors.length == 0) {
    output.innerHTML = JSON.stringify(ast, (key, value) => (key == "source" ? undefined : value), 2).replace(/\n/g, "<br>");
  } else {
    output.innerHTML = errors
      .map((error) => error.toString())
      .reduce((prev, curr) => prev + curr)
      .replace(/\n/g, "<br>");
  }
}

const stored = localStorage.getItem("source");
if (stored) {
  source.value = stored;
  tokenize(source.value);
}

source.addEventListener("input", () => {
  tokenize(source.value);
});

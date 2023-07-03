const { tokenizer } = require("../lib");

const source = document.querySelector("#source");
const output = document.querySelector("#output");

function tokenize(value) {
  localStorage.setItem("source", value);
  const sourceDoc = new tokenizer.Source("source", value);
  const { tokens, errors } = tokenizer.tokenize(sourceDoc);
  if (errors.length == 0) {
    output.innerHTML = tokens.map((token) => JSON.stringify(token, null, 2)).join("<br>");
  } else {
    output.innerHTML = sourceDoc
      .printErrors(errors)
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

const { tokenizer } = require("../lib");

const source = document.querySelector("#source");
const output = document.querySelector("#output");

source.addEventListener("input", () => {
  const { tokens, errors } = tokenizer.tokenize({ id: "source", text: source.value });
  if (errors.length == 0) {
    output.innerHTML = tokens.map((token) => JSON.stringify(token, null, 2)).join("<br>");
  } else {
    output.innerHTML = errors.map((error) => JSON.stringify(error, null, 2)).join("<br>");
  }
});

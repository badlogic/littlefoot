const { tokenizer } = require("../lib");

const source = document.querySelector("#source");
const output = document.querySelector("#output");

source.addEventListener("input", () => {
  output.innerHTML = tokenizer
    .tokenize({ id: "source", text: source.value })
    .map((token) => {
      JSON.stringify(token, null, 2);
    })
    .join("<br>");
});

const tokenizer = require("./lib/tokenizer");

const source = {
  id: "source",
  text: `This is a test अ "👨‍👩‍👧🇺🇸😀test" < >= = ([==)`,
};

const { tokens, errors } = tokenizer.tokenize(source);
for (const token of tokens) {
  console.log(token);
}

const tokenizer = require("./lib/tokenizer");

const source = {
  id: "source",
  text: `This is a test अ "👨‍👩‍👧🇺🇸😀\\rte \\\" \\ns\\tt" < >= = ([==) 123 123.545 012 0xff 0b101`,
};

const { tokens, errors } = tokenizer.tokenize(source);
for (const token of tokens) {
  console.log(token);
}

import { LittleFootError } from "./error.js";

export class Token {
  constructor(start, end, value, source) {
    this.start = start;
    this.end = end;
    this.value = value;
    this.source = source;
  }
}

export class NumberToken extends Token {
  static type = "number";
}

export class StringToken extends Token {
  static type = "string";
}

export class IdentifierToken extends Token {
  static type = "identifier";
}

export class OperatorToken extends Token {
  static type = "operator";
}

const isLetterRegex = /\p{Alphabetic}/u;

function isLetter(char) {
  return isLetterRegex.test(char);
}

function isDigit(char) {
  return char >= "0" && char <= "9";
}

function isHexDigit(char) {
  return isDigit(char) || (char >= "a" && char <= "f") || (char >= "A" && char <= "F");
}

function isBinaryDigit(char) {
  return char == "0" || char == "1";
}

function isIdentifierStart(char) {
  return isLetter(char) || char === "_";
}

function isIdentifierPart(char) {
  return isLetter(char) || isDigit(char) || char === "_";
}

let operators = ["=", "|", "&", "^", "==", "!=", ">", ">=", "<", "<=", "+", "-", "*", "/", "%", "(", ")", "[", "]", ".", "?", ":"].sort(
  (a, b) => b.length - a.length
);
const operatorStarts = new Set(operators.map((operator) => operator.charAt(0)));
operators = new Set(operators);

export function tokenize(source) {
  const text = source.text;
  let tokens = [];
  let errors = [];
  for (let i = 0, n = text.length; i < n; ) {
    const char = text.charAt(i);

    // ignore whitespace
    if (char == "\r" || char == " " || char == "\t") {
      i++;
      continue;
    }

    // newline
    if (char == "\n") {
      i++;
      continue;
    }

    // operators
    if (operatorStarts.has(char)) {
      let start = i;
      i++;
      if (operators.has(char)) {
        if (i == n) {
          tokens.push(new OperatorToken(start, i, char, source));
        } else {
          if (operators.has(char + text.charAt(i))) {
            tokens.push(new OperatorToken(start, i, char + text.charAt(i), source));
            i++;
          } else {
            tokens.push(new OperatorToken(start, i, char, source));
          }
        }
      }
      continue;
    }

    // numbers
    if (isDigit(char)) {
      let start = i;
      let value = char;
      i++;

      if (i == n) {
        tokens.push(new NumberToken(start, i, Number.parseInt(char), source));
        continue;
      }

      // hex
      if (char == "0" && text.charAt(i) == "x") {
        value = "";
        i++;
        if (i == n) {
          errors.push(new LittleFootError(start, i, source, `Source ended before hexadecimal number was complete.`));
          return { tokens, errors };
        }
        while (i < n && isHexDigit(text.charAt(i))) {
          value += text.charAt(i);
          i++;
        }
        if (value.length == 0) {
          errors.push(new LittleFootError(start, i, source, `Expected one or more hexadecimal digits.`));
          return { tokens, errors };
        }
        tokens.push(new NumberToken(start, i, Number.parseInt(value, 16)));
        continue;
      } else if (char == "0" && text.charAt(i) == "b") {
        value = "";
        i++;
        if (i == n) {
          errors.push(new LittleFootError(start, i, source, `Source ended before binary number was complete.`));
          return { tokens, errors };
        }
        while (i < n && isBinaryDigit(text.charAt(i))) {
          value += text.charAt(i);
          i++;
        }
        if (value.length == 0) {
          errors.push(new LittleFootError(start, i, source, `Expected one or more binary digits.`));
          return { tokens, errors };
        }
        tokens.push(new NumberToken(start, i, Number.parseInt(value, 2)));
        continue;
      } else {
        while (i < n && isDigit(text.charAt(i))) {
          value += text.charAt(i);
          i++;
        }
        if (i == n) {
          tokens.push(new NumberToken(start, i, Number.parseFloat(value), source));
          continue;
        }

        if (text.charAt(i) == ".") {
          value += ".";
          i++;
          while (i < n && isDigit(text.charAt(i))) {
            value += text.charAt(i);
            i++;
          }
        }
        tokens.push(new NumberToken(start, i, Number.parseFloat(value), source));
        continue;
      }
    }

    // strings
    if (char == '"') {
      let start = i;
      i++;
      let value = "";
      let closed = false;
      while (i < n) {
        const char = text.charAt(i);
        i++;
        if (char == "\\") {
          if (i == n) {
            errors.push(new LittleFootError(start, i, source, `Source ends before string escape and string was closed.`));
            return { tokens, errors };
          }
          const escaped = text.charAt(i++);
          if (escaped === "t") {
            value += "\t";
          } else if (escaped === "r") {
            value += "\r";
          } else if (escaped === "n") {
            value += "\n";
          } else if (escaped === '"') {
            value += '"';
          } else {
            errors.push(new LittleFootError(start, i, source, `Unknown string escape.`));
            return { tokens, errors };
          }
        } else if (char == '"') {
          closed = true;
          tokens.push(new StringToken(start, i, value, source));
          break;
        } else {
          value += char;
        }
      }
      if (!closed) {
        errors.push(new LittleFootError(start, i, source, `String not closed.`));
        return { tokens, errors };
      }
      continue;
    }

    // identifiers and keywords
    if (isIdentifierStart(char)) {
      let start = i;
      i++;
      while (i < n && isIdentifierPart(text.charAt(i))) {
        i++;
      }
      tokens.push(new IdentifierToken(start, i, source.text.substring(start, i), source));
      continue;
    }

    errors.push(new LittleFootError(i, i + 1, source, `Unknown token.`));
    return { tokens, errors };
  }
  return { tokens, errors };
}

export class TokenStream {
  constructor(source, tokens) {
    this.source = source;
    this.tokens = tokens;
    this.index = 0;
  }

  next() {
    if (!this.hasMore()) throw new Error("Reached end of token stream. This should never happen.");
    return this.tokens[this.index++];
  }

  hasMore() {
    return this.index < this.tokens.length;
  }

  matchValue(value, consume) {
    if (!this.hasMore()) return false;
    const result = this.tokens[this.index].value === value;
    if (result && consume) this.next();
    return result;
  }

  matchValues(values, consume) {
    if (!this.hasMore()) return false;
    let result = false;
    const token = this.tokens[this.index];
    for (const value of values) {
      if (value == token.value) {
        result = true;
        break;
      }
    }
    if (result && consume) this.next();
    return result;
  }

  expectValue(value) {
    if (!this.hasMore()) {
      throw new LittleFootError(this.source.text.length, this.source.text.length, this.source, `Expected '${value}' but reached end of file.`);
    }
    const token = this.tokens[this.index];
    const result = token.value === value;
    if (!result) {
      throw new LittleFootError(token.start, token.end, this.source, `Expected '${value}' but got '${this.tokens[this.index].value}'`);
    }
    return this.next();
  }

  expectValues(values) {
    if (!this.hasMore()) {
      throw new LittleFootError(this.source.text.length, this.source.text.length, this.source, `Expected '${values}' but reached end of file.`);
    }
    let result = false;
    const token = this.tokens[this.index];
    for (const value of values) {
      if (value == token.value) {
        result = true;
        break;
      }
    }
    if (!result) {
      throw new LittleFootError(token.start, token.end, this.source, `Expected '${values}' but got '${token.value}'`);
    }
    return this.next();
  }

  matchType(type, consume) {
    if (!this.hasMore()) return false;
    const result = this.tokens[this.index] instanceof type;
    if (result && consume) this.next();
    return result;
  }

  expectType(type) {
    if (!this.hasMore()) {
      throw new LittleFootError(this.source.text.length, this.source.text.length, this.source, `Expected '${type.type}' but reached end of file.`);
    }
    const token = this.tokens[this.index];
    const result = token instanceof type;
    if (!result) {
      throw new LittleFootError(token.start, token.end, this.source, `Expected '${type.type}' but got '${token.value}'`);
    }
    return this.next();
  }

  expectTypes(types) {
    if (!this.hasMore()) {
      throw new LittleFootError(
        this.source.text.length,
        this.source.text.length,
        this.source,
        `Expected '${types.map((type) => type.type).join(" or ")}' but reached end of file.`
      );
    }
    let result = false;
    const token = this.tokens[this.index];
    for (const type of types) {
      if (token instanceof type) {
        result = true;
        break;
      }
    }
    if (!result) {
      throw new LittleFootError(
        token.start,
        token.end,
        this.source,
        `Expected '${types.map((type) => type.type).join(" or ")}' but got '${token.value}'`
      );
    }
    return this.next();
  }
}

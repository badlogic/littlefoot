import { LittleFootError } from "./error.js";

export class Token {
  constructor(type, start, end, value, source) {
    this.type = type;
    this.start = start;
    this.end = end;
    this.value = value;
    this.source = source;
  }
}

export class NumberToken extends Token {
  constructor(start, end, value, source) {
    super("number", start, end, value, source);
  }
}

export class StringToken extends Token {
  constructor(start, end, value, source) {
    super("string", start, end, value, source);
  }
}

export class IdentifierToken extends Token {
  constructor(start, end, value, source) {
    super("identifier", start, end, value, source);
  }
}

export class OperatorToken extends Token {
  constructor(start, end, value, source) {
    super("operator", start, end, value, source);
  }
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

let operators = [">", ">=", "<", "<=", "==", "!=", "+", "-", "*", "/", "%", "(", ")", "[", "]", ".", "="].sort((a, b) => b.length - a.length);
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

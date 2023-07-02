class Source {
  constructor(identifier, text) {
    this.identifier = identifier;
    this.text = text;
    this.lines = [];
  }
}
exports.Source = Source;

class Token {
  constructor(type, line, start, end, value, source) {
    this.type = type;
    this.line = line;
    this.start = start;
    this.end = end;
    this.value = value;
    this.source = source;
  }
}
exports.Token = Token;

class Error {
  constructor(line, start, end, source, message) {
    this.line = line;
    this.start = start;
    this.end = end;
    this.source = source;
    this.message = message;
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

exports.tokenize = (source) => {
  const text = source.text;
  let line = 0;
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
      line++;
      i++;
      continue;
    }

    // operators
    if (operatorStarts.has(char)) {
      let start = i;
      i++;
      if (operators.has(char)) {
        if (i == n) {
          tokens.push(new Token("operator", line, start, i, char, source));
        } else {
          if (operators.has(char + text.charAt(i))) {
            tokens.push(new Token("operator", line, start, i, char + text.charAt(i), source));
            i++;
          } else {
            tokens.push(new Token("operator", line, start, i, char, source));
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
        tokens.push(new Token("number", line, start, i, Number.parseInt(char), source));
        continue;
      }

      // hex
      if (char == "0" && text.charAt(i) == "x") {
        value = "";
        i++;
        if (i == n) {
          errors.push(new Error(line, start, i, source, `Source ended before hexadecimal number was complete.`));
          return { token, errors };
        }
        while (i < n && isHexDigit(text.charAt(i))) {
          value += text.charAt(i);
          i++;
        }
        if (value.length == 0) {
          errors.push(new Error(line, start, i, source, `Expected one or more hexadecimal digits.`));
          return { token, errors };
        }
        tokens.push(new Token("number", line, start, i, Number.parseInt(value, 16)));
        continue;
      } else if (char == "0" && text.charAt(i) == "b") {
        value = "";
        i++;
        if (i == n) {
          errors.push(new Error(line, start, i, source, `Source ended before binary number was complete.`));
          return { token, errors };
        }
        while (i < n && isBinaryDigit(text.charAt(i))) {
          value += text.charAt(i);
          i++;
        }
        if (value.length == 0) {
          errors.push(new Error(line, start, i, source, `Expected one or more binary digits.`));
          return { token, errors };
        }
        tokens.push(new Token("number", line, start, i, Number.parseInt(value, 2)));
        continue;
      } else {
        while (i < n && isDigit(text.charAt(i))) {
          value += text.charAt(i);
          i++;
        }
        if (i == n) {
          tokens.push(new Token("number", line, start, i, Number.parseFloat(value), source));
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
        tokens.push(new Token("number", line, start, i, Number.parseFloat(value), source));
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
            errors.push(new Error(line, start, i, source, `Source ends before string escape and string was closed.`));
            return { token, errors };
          }
          const escaped = text.charAt(i++);
          if (escaped === "t") {
            result += "\t";
          } else if (escaped === "r") {
            result += "\r";
          } else if (escaped === "n") {
            result += "\n";
          } else if (escaped === '"') {
            result += '"';
          } else {
            errors.push(new Error(line, start, i, source, `Unknown string escape.`));
            return { tokens, errors };
          }
        } else if (char == '"') {
          closed = true;
          tokens.push(new Token("string", line, start, i, value, source));
          break;
        } else {
          value += char;
        }
      }
      if (!closed) {
        errors.push(new Error(line, start, i, source, `String not closed.`));
        return { token, errors };
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
      tokens.push(new Token("identifier", line, start, i, source.text.substring(start, i), source));
      continue;
    }

    errors.push(new Error(line, i, i + 1, source, `Unknown token.`));
    return { tokens, errors };
  }
  return { tokens, errors };
};

class Source {
  constructor(identifier, text) {
    this.identifier = identifier;
    this.text = text;
    this.lines = [];
    let line = {
      index: 1,
      start: 0,
      end: 0,
    };
    let i = 0;
    for (; i < text.length; i++) {
      const char = text.charAt(i);
      if (char == "\n") {
        line.end = i + 1;
        this.lines.push(line);
        if (i == text.length - 1) {
          line = null;
        } else {
          line = {
            index: line.index + 1,
            start: line.end,
            end: line.end + 1,
          };
        }
      }
    }
    if (line) {
      line.end = i + 1;
      this.lines.push(line);
    }
  }

  indicesToLines(start, end) {
    const startLine = this.lines.findIndex((line) => start >= line.start && start < line.end);
    const endLine = this.lines.findIndex((line) => end >= line.start && end < line.end);
    return this.lines.slice(startLine, endLine + 1);
  }

  printErrors(errors) {
    const highlights = [];
    for (const error of errors) {
      const lines = this.indicesToLines(error.start, error.end);
      let highlight = `${this.identifier}:${lines[0].index}: ${error.message}\n\n`;
      let index = lines[0].start;
      for (const line of lines) {
        const lineText = this.text.substring(line.start, line.end);
        highlight += lineText.replace("\n", "") + "\n";
        for (let i = 0; i < lineText.length; i++) {
          const char = lineText.charAt(i);
          if (index >= error.start && index < error.end) {
            highlight += "^";
          } else {
            highlight += char == "\t" ? "\t" : " ";
          }
          index++;
        }
      }

      highlights.push(highlight + "\n");
    }
    return highlights;
  }
}
exports.Source = Source;

class Token {
  constructor(type, start, end, value, source) {
    this.type = type;
    this.start = start;
    this.end = end;
    this.value = value;
    this.source = source;
  }
}
exports.Token = Token;

class Error {
  constructor(start, end, source, message) {
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
          tokens.push(new Token("operator", start, i, char, source));
        } else {
          if (operators.has(char + text.charAt(i))) {
            tokens.push(new Token("operator", start, i, char + text.charAt(i), source));
            i++;
          } else {
            tokens.push(new Token("operator", start, i, char, source));
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
        tokens.push(new Token("number", start, i, Number.parseInt(char), source));
        continue;
      }

      // hex
      if (char == "0" && text.charAt(i) == "x") {
        value = "";
        i++;
        if (i == n) {
          errors.push(new Error(start, i, source, `Source ended before hexadecimal number was complete.`));
          return { tokens, errors };
        }
        while (i < n && isHexDigit(text.charAt(i))) {
          value += text.charAt(i);
          i++;
        }
        if (value.length == 0) {
          errors.push(new Error(start, i, source, `Expected one or more hexadecimal digits.`));
          return { tokens, errors };
        }
        tokens.push(new Token("number", start, i, Number.parseInt(value, 16)));
        continue;
      } else if (char == "0" && text.charAt(i) == "b") {
        value = "";
        i++;
        if (i == n) {
          errors.push(new Error(start, i, source, `Source ended before binary number was complete.`));
          return { tokens, errors };
        }
        while (i < n && isBinaryDigit(text.charAt(i))) {
          value += text.charAt(i);
          i++;
        }
        if (value.length == 0) {
          errors.push(new Error(start, i, source, `Expected one or more binary digits.`));
          return { tokens, errors };
        }
        tokens.push(new Token("number", start, i, Number.parseInt(value, 2)));
        continue;
      } else {
        while (i < n && isDigit(text.charAt(i))) {
          value += text.charAt(i);
          i++;
        }
        if (i == n) {
          tokens.push(new Token("number", start, i, Number.parseFloat(value), source));
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
        tokens.push(new Token("number", start, i, Number.parseFloat(value), source));
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
            errors.push(new Error(start, i, source, `Source ends before string escape and string was closed.`));
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
            errors.push(new Error(start, i, source, `Unknown string escape.`));
            return { tokens, errors };
          }
        } else if (char == '"') {
          closed = true;
          tokens.push(new Token("string", start, i, value, source));
          break;
        } else {
          value += char;
        }
      }
      if (!closed) {
        errors.push(new Error(start, i, source, `String not closed.`));
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
      tokens.push(new Token("identifier", start, i, source.text.substring(start, i), source));
      continue;
    }

    errors.push(new Error(i, i + 1, source, `Unknown token.`));
    return { tokens, errors };
  }
  return { tokens, errors };
};

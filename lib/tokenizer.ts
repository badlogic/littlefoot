import { LittleFootError } from "./error";
import { Source } from "./source";

export abstract class Token {
  constructor(
    public readonly start: number,
    public readonly end: number,
    public readonly value: number | string,
    public readonly source: Source,
    public readonly comments: CommentToken[] = []
  ) {}
}

export class NothingToken extends Token {}
export class BoolToken extends Token {}
export class NumberToken extends Token {}
export class StringToken extends Token {}
export class IdentifierToken extends Token {}
export class KeywordToken extends Token {}
export class OperatorToken extends Token {}
export class CommentToken extends Token {}
export class TupleOpeningToken extends Token {}
export class TupleClosingToken extends Token {}

type TokenConstructor<T extends Token> = abstract new (...args: any[]) => T;

function tokenLabel<T extends Token>(tokenType: TokenConstructor<T>) {
  if (tokenType.name == "NothingToken") return "nothing";
  if (tokenType.name == "BoolToken") return "boolean";
  if (tokenType.name == "NumberToken") return "number";
  if (tokenType.name == "StringToken") return "string";
  if (tokenType.name == "IdentifierToken") return "identifier";
  if (tokenType.name == "KeywordToken") return "keyword";
  if (tokenType.name == "OperatorToken") return "operator";
  if (tokenType.name == "CommentToken") return "comment";
  throw new Error("Unknown token type: " + tokenType.name);
}

const isLetterRegex = /\p{Alphabetic}/u;

function isLetter(char: string) {
  return isLetterRegex.test(char);
}

function isDigit(char: string) {
  return char >= "0" && char <= "9";
}

function isHexDigit(char: string) {
  return isDigit(char) || (char >= "a" && char <= "f") || (char >= "A" && char <= "F");
}

function isBinaryDigit(char: string) {
  return char == "0" || char == "1";
}

function isIdentifierStart(char: string) {
  return isLetter(char) || char === "_";
}

function isIdentifierPart(char: string) {
  return isLetter(char) || isDigit(char) || char === "_";
}

export const keywords = [
  "var",
  "func",
  "type",
  "if",
  "then",
  "elseif",
  "else",
  "while",
  "do",
  "for",
  "from",
  "each",
  "to",
  "step",
  "end",
  "continue",
  "break",
  "return",
];
let keywordsLookup = new Set(keywords);
// prettier-ignore
export const operatorsList = [ "=", "!", "|", "&", "^", "==", "!=", ">", ">=", "<", "<=", "+", "-", "*", "/", "%", "(", ")", "[", "]", ".", "?", ":", ",", "{", "}", "is", ";"].sort((a, b) => b.length - a.length);
const operatorStarts = new Set(operatorsList.map((operator) => operator.charAt(0)));
const operators = new Set(operatorsList);

export function tokenize(source: Source) {
  const text = source.text;
  let tokens: Token[] = [];
  let errors: LittleFootError[] = [];
  let comments = [];
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

    // comments
    if (char == "#") {
      const start = i;
      let value = "";
      let c = char;
      while (c != "\n" && i < n) {
        c = text.charAt(i);
        value += c;
        i++;
      }
      comments.push(new CommentToken(start, i, value, source));
      continue;
    }

    // operators
    if (operatorStarts.has(char)) {
      let start = i;
      if (i == n && operators.has(char)) {
        tokens.push(new OperatorToken(start, i, char, source, comments));
        comments = [];
        continue;
      }
      if (operators.has(char + text.charAt(i + 1))) {
        tokens.push(new OperatorToken(start, i + 2, char + text.charAt(i + 1), source, comments));
        comments = [];
        i += 2;
        continue;
      }
      if (operators.has(char)) {
        tokens.push(new OperatorToken(start, i + 1, char, source, comments));
        comments = [];
        i++;
        continue;
      }
    }

    // numbers
    if (isDigit(char)) {
      let start = i;
      let value = char;
      i++;

      if (i == n) {
        tokens.push(new NumberToken(start, i, Number.parseInt(char), source, comments));
        comments = [];
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
        tokens.push(new NumberToken(start, i, Number.parseInt(value, 16), source, comments));
        comments = [];
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
        tokens.push(new NumberToken(start, i, Number.parseInt(value, 2), source, comments));
        comments = [];
        continue;
      } else {
        while (i < n && isDigit(text.charAt(i))) {
          value += text.charAt(i);
          i++;
        }
        if (i == n) {
          tokens.push(new NumberToken(start, i, Number.parseFloat(value), source, comments));
          comments = [];
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
        tokens.push(new NumberToken(start, i, Number.parseFloat(value), source, comments));
        comments = [];
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
          tokens.push(new StringToken(start, i, value, source, comments));
          comments = [];
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
      const identifier = source.text.substring(start, i);
      if (identifier == "true" || identifier == "false") {
        tokens.push(new BoolToken(start, i, identifier, source, comments));
      } else if (identifier == "nothing") {
        tokens.push(new NothingToken(start, i, identifier, source, comments));
      } else if (keywordsLookup.has(identifier)) {
        tokens.push(new KeywordToken(start, i, identifier, source, comments));
      } else {
        tokens.push(new IdentifierToken(start, i, identifier, source, comments));
      }
      comments = [];
      continue;
    }

    errors.push(new LittleFootError(i, i + 1, source, `Unknown token.`));
    return { tokens, errors };
  }

  // Post process the tokens and find the pattern "< identifier :"
  // rewrite the < operator token to be a tuple literal opening token.
  for (let i = 0; i < tokens.length - 2; i++) {
    if (tokens[i].value == "<" && tokens[i + 1] instanceof IdentifierToken && tokens[i + 2].value == ":") {
      const token = tokens[i];
      tokens[i] = new TupleOpeningToken(token.start, token.end, token.value + "|", token.source, token.comments);
    }
  }

  return { tokens, errors };
}

export class TokenStream {
  constructor(public readonly source: Source, public readonly tokens: Token[], public index: number = 0) {}

  next<T extends Token>() {
    if (!this.hasMore()) throw new Error("Reached end of token stream. This should never happen.");
    return this.tokens[this.index++] as T;
  }

  hasMore() {
    return this.index < this.tokens.length;
  }

  matchValue(value: string, consume = false) {
    if (!this.hasMore()) return false;
    const result = this.tokens[this.index].value === value;
    if (result && consume) this.next();
    return result;
  }

  matchValues(values: string[], consume = false) {
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

  expectValue(value: string) {
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

  expectValues(values: string[]) {
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

  matchType<T extends Token>(type: TokenConstructor<T>, consume = false) {
    if (!this.hasMore()) return false;
    const result = this.tokens[this.index] instanceof type;
    if (result && consume) this.next();
    return result;
  }

  expectType<T extends Token>(type: TokenConstructor<T>): T {
    if (!this.hasMore()) {
      throw new LittleFootError(
        this.source.text.length,
        this.source.text.length,
        this.source,
        `Expected '${tokenLabel(type)}' but reached end of file.`
      );
    }
    const token = this.tokens[this.index];
    const result = token instanceof type;
    if (!result) {
      throw new LittleFootError(token.start, token.end, this.source, `Expected '${tokenLabel(type)}' but got '${token.value}'`);
    }
    return this.next<T>();
  }
}

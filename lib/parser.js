import { LittleFootError } from "./error.js";
import { tokenize, Token, IdentifierToken, StringToken, NumberToken } from "./tokenizer.js";

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

  expectValue(value) {
    if (!this.hasMore()) {
      throw new LittleFootError(this.source.text.length, this.source.text.length, this.source, `Expected '${value}' but reached end of file.`);
    }
    const result = this.tokens[this.index].value === value;
    if (!result) {
      throw new LittleFootError(
        this.source.text.length,
        this.source.text.length,
        this.source,
        `Expected '${value}' but got '${tokens[this.tokens.length - 1].value}'`
      );
    }
    return this.next();
  }

  expectValues(values) {
    if (!this.hasMore()) {
      throw new LittleFootError(this.source.text.length, this.source.text.length, this.source, `Expected '${values}' but reached end of file.`);
    }
    let result = false;
    const tokenValue = this.tokens[this.index].value;
    for (const value of values) {
      if (value == tokenValue) {
        result = true;
        break;
      }
    }
    if (!result) {
      throw new LittleFootError(this.source.text.length, this.source.text.length, this.source, `Expected '${values}' but got '${tokenValue}'`);
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
      throw new LittleFootError(this.source.text.length, this.source.text.length, this.source, `Expected '${type.name}' but reached end of file.`);
    }
    const result = this.tokens[this.index] instanceof type;
    if (!result) {
      throw new LittleFootError(
        this.source.text.length,
        this.source.text.length,
        this.source,
        `Expected '${type.name}' but got '${tokens[this.tokens.length - 1].name}'`
      );
    }
    return this.next();
  }

  expectTypes(types) {
    if (!this.hasMore()) {
      throw new LittleFootError(
        this.source.text.length,
        this.source.text.length,
        this.source,
        `Expected '${types.map((type) => type.name).join(" or ")}' but reached end of file.`
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
        this.source.text.length,
        this.source.text.length,
        this.source,
        `Expected '${types.map((type) => type.name).join(" or ")}' but got '${token.name}'`
      );
    }
    return this.next();
  }
}

export function parse(source) {
  let ast = [];
  const { tokens, errors } = tokenize(source);
  if (errors.length > 0) return { ast, errors };

  const stream = new TokenStream(source, tokens);

  try {
    while (stream.hasMore()) {
      if (stream.matchValue("var")) {
        const variable = parseVariable(stream);
        if (errors.length > 0) break;
        ast.push(variable);
      } else {
        const token = stream.next();
        errors.push(LittleFootError.fromTokens([token], "Unexpected token."));
        break;
      }
    }
  } catch (e) {
    if (e instanceof LittleFootError) errors.push(e);
    else errors.push(new LittleFootError(0, 1, source, "Internal error: " + e.message + "\n" + e.stack));
  } finally {
    return { ast, errors };
  }
}

class AstNode {}

class VariableNode extends AstNode {
  constructor(identifier, initializer, type) {
    super();
    this.identifier = identifier;
    this.initializer = initializer;
    this.type = type;
  }
}

function parseVariable(stream) {
  stream.expectValue("var");
  const identifier = stream.expectType(IdentifierToken);
  const type = stream.matchValue(":", true) ? stream.expectType(IdentifierToken) : null;
  stream.expectValue("=");
  const initializer = parseExpression(stream);
  return new VariableNode(identifier, initializer, type);
}

function parseExpression(stream) {
  return stream.expectTypes([StringToken, NumberToken]);
}

import { LittleFootError } from "./error.js";
import { tokenize, TokenStream, IdentifierToken, StringToken, NumberToken } from "./tokenizer.js";

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
        ast.push(parseExpression(stream));
      }
    }
  } catch (e) {
    if (e instanceof LittleFootError) errors.push(e);
    else errors.push(new LittleFootError(0, 1, source, "Internal error: " + e.message + "\n" + e.stack));
  } finally {
    return { ast, errors };
  }
}

export class AstNode {}

export class VariableNode extends AstNode {
  constructor(identifier, initializer, type) {
    super();
    this.nodeType = "variable declaration";
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
  return parseTernaryOperator(stream);
}

export class TernaryOperatorNode extends AstNode {
  constructor(condition, trueExpression, falseExpression) {
    super();
    this.nodeType = "ternary operator";
    this.condition = condition;
    this.trueExpression = trueExpression;
    this.falseExpression = falseExpression;
  }
}

function parseTernaryOperator(stream) {
  const condition = parseBinaryOperator(stream, 0);
  if (stream.matchValue("?", true)) {
    const trueExpression = parseTernaryOperator(stream);
    stream.expectValue(":");
    const falseExpression = parseTernaryOperator(stream);
    return TernaryOperatorNode(condition, trueExpression, falseExpression);
  } else {
    return condition;
  }
}

const binaryOperatorPrecedence = [["="], ["|", "&", "^"], ["==", "!="], ["<", "<=", ">", ">="], ["+", "-"], ["/", "*", "%"]];

export class BinaryOperatorNode extends AstNode {
  constructor(leftExpression, operator, rightExpression) {
    super();
    this.nodeType = "binary operator";
    this.leftExpression = leftExpression;
    this.operator = operator;
    this.rightExpression = rightExpression;
  }
}

function parseBinaryOperator(stream, level) {
  const nextLevel = level + 1;
  let leftExpression = nextLevel == binaryOperatorPrecedence.length ? parseUnaryOperator(stream) : parseBinaryOperator(stream, nextLevel);
  const operators = binaryOperatorPrecedence[level];
  while (stream.hasMore() && stream.matchValues(operators, false)) {
    const operator = stream.next();
    const rightExpression = nextLevel == binaryOperatorPrecedence.length ? parseUnaryOperator(stream) : parseBinaryOperator(stream, nextLevel);
    leftExpression = new BinaryOperatorNode(leftExpression, operator, rightExpression);
  }
  return leftExpression;
}

const unaryOperators = ["!", "+", "-"];

function parseUnaryOperator(stream) {
  if (stream.matchValues(unaryOperators, false)) {
    return new UnaryOperatorNode(stream.next(), parseUnaryOperator(stream));
  } else {
    if (stream.matchValue("(", true)) {
      const expression = parseExpression(stream);
      stream.expectValue(")");
      return expression;
    } else {
      return parseAccessOrCallOrLiteral(stream);
    }
  }
}

export class StringLiteralNode extends AstNode {
  constructor(token) {
    super();
    this.nodeType = "string";
    this.token = token;
  }
}

export class NumberLiteralNode extends AstNode {
  constructor(token) {
    super();
    this.nodeType = "number";
    this.token = token;
  }
}

export class BooleanLiteralNode extends AstNode {
  constructor(token) {
    super();
    this.nodeType = "boolean";
    this.token = token;
  }
}

function parseAccessOrCallOrLiteral(stream) {
  if (stream.matchValue("{")) {
    throw Error("Map literal not implemented");
  } else if (stream.matchValue("[")) {
    throw Error("List literal not implemented");
  } else if (stream.matchType(StringToken)) {
    return new StringLiteralNode(stream.next());
  } else if (stream.matchType(NumberToken)) {
    return new NumberLiteralNode(stream.next());
  } else if (stream.matchValues(["true", "false"])) {
    return new BooleanLiteralNode(stream.next());
  } else if (stream.matchType(IdentifierToken)) {
    return parseAccessOrCall(stream);
  } else {
    if (stream.hasMore()) {
      const token = stream.next();
      throw new LittleFootError(
        token.start,
        token.end,
        stream.source,
        `Expected a string, number, boolean, variable, field, map, array, function or method call, but got ${token.value}.`
      );
    } else {
      throw new LittleFootError(
        stream.source.text.length,
        stream.source.text.length,
        stream.source,
        "Expected a string, number, boolean, variable, field, map, array, function or method call, but reached end of file."
      );
    }
  }
}

export class VariableAccessNode extends AstNode {
  constructor(variable) {
    super();
    this.nodeType = "variable access";
    this.variable = variable;
  }
}

export class MemberAccessNode extends AstNode {
  constructor(object, member) {
    super();
    this.nodeType = "member access";
    this.object = object;
    this.member = member;
  }
}

export class MapOrArrayAccessNode extends AstNode {
  constructor(target, keyOrIndex) {
    super();
    this.nodeType = "map or array access";
    this.target = target;
    this.keyOrIndex = keyOrIndex;
  }
}

export class FunctionCallNode extends AstNode {
  constructor(target, args) {
    super();
    this.nodeType = "function call";
    this.target = target;
    this.args = args;
  }
}

export class MethodCallNode extends AstNode {
  constructor(target, args) {
    super();
    this.nodeType = "method call";
    this.target = target;
    this.args = args;
  }
}

function parseAccessOrCall(stream) {
  let result = new VariableAccessNode(stream.expectType(IdentifierToken));
  while (stream.hasMore() && stream.matchValues(["(", "[", "."])) {
    if (stream.matchValue("(")) {
      const openingParanthesis = stream.tokens[stream.index];
      const args = parseArguments(stream);
      const closingParanthesis = stream.tokens[stream.index - 1];
      if (result instanceof VariableAccessNode || result instanceof MapOrArrayAccessNode) {
        result = new FunctionCallNode(result, args);
      } else if (result instanceof MemberAccessNode) {
        result = new MethodCallNode(result, args);
      } else {
        throw new LittleFootError(openingParanthesis.start, closingParanthesis.end, stream.source, `Expected a variable, field, or method.`);
      }
    } else if (stream.matchValue("[", true)) {
      const keyOrIndex = parseExpression(stream);
      stream.expectValue("]");
      result = new MapOrArrayAccessNode(result, keyOrIndex);
    } else if (stream.matchValue(".", true)) {
      const identifier = stream.expectType(IdentifierToken);
      result = new MemberAccessNode(result, identifier);
    }
  }
  return result;
}

function parseArguments(stream) {
  stream.expectValue("(");
  const args = [];
  while (stream.hasMore() && !stream.matchValue(")", true)) {
    args.push(parseExpression(stream));
    if (!stream.matchValue(")")) stream.expectValue(",");
  }
  return args;
}

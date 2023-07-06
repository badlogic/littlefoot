import { LittleFootError } from "./error.js";
import { tokenize, TokenStream, IdentifierToken, StringToken, NumberToken, CommentToken, NothingToken } from "./tokenizer.js";
// prettier-ignore
import { ArrayLiteralNode, ArrayTypeNode, BinaryOperatorNode, BooleanLiteralNode, CommentNode, DoNode, ForEachNode, ForNode, FunctionCallNode, FunctionNode, FunctionTypeNode, IfNode, IsOperatorNode, MapLiteralNode, MapOrArrayAccessNode, MapTypeNode, MemberAccessNode, MethodCallNode, NameAndTypeNode, NothingLiteralNode, NumberLiteralNode, PlainTypeNode, StringLiteralNode, TernaryOperatorNode, TypeDeclarationNode, UnaryOperatorNode, VariableAccessNode, VariableNode, WhileNode } from "./ast.js";

export function parse(source) {
  let ast = [];
  const { tokens, errors } = tokenize(source);
  if (errors.length > 0) return { ast, errors };

  const stream = new TokenStream(source, tokens);

  try {
    while (stream.hasMore()) {
      ast.push(parseStatement(stream));
    }
  } catch (e) {
    if (e instanceof LittleFootError) errors.push(e);
    else errors.push(new LittleFootError(0, 1, source, "Internal error: " + e.message + "\n" + e.stack));
  } finally {
    return { ast, errors };
  }
}

function parseStatement(stream) {
  if (stream.matchValue("var")) {
    return parseVariable(stream);
  } else if (stream.matchValue("type")) {
    return parseTypeDeclaration(stream);
  } else if (stream.matchValue("func")) {
    return parseFunction(stream, true);
  } else if (stream.matchValue("if")) {
    return parseIf(stream);
  } else if (stream.matchValue("while")) {
    return parseWhile(stream);
  } else if (stream.matchValue("do")) {
    return parseDo(stream);
  } else if (stream.matchValue("for")) {
    return parseFor(stream);
  } else if (stream.matchType(CommentToken)) {
    const lines = [];
    while (stream.matchType(CommentToken)) {
      lines.push(stream.next());
    }
    return new CommentNode(lines);
  } else {
    return parseExpression(stream);
  }
}

function parseVariable(stream) {
  stream.expectValue("var");
  const identifier = stream.expectType(IdentifierToken);
  const type = stream.matchValue(":", true) ? parseTypeSpecifier(stream) : null;
  stream.expectValue("=");
  const initializer = parseExpression(stream);
  return new VariableNode(identifier, initializer, type);
}

function parseTypeSpecifier(stream) {
  const type = [];
  do {
    if (stream.matchType(IdentifierToken) || stream.matchType(NothingToken)) {
      type.push(new PlainTypeNode(stream.next()));
    } else if (stream.matchValue("[", true)) {
      type.push(new ArrayTypeNode(parseTypeSpecifier(stream)));
      stream.expectValue("]");
    } else if (stream.matchValue("{", true)) {
      type.push(new MapTypeNode(parseTypeSpecifier(stream)));
      stream.expectValue("}");
    } else if (stream.matchValue("(")) {
      const { parameters, returnType } = parseFunctionSignature(stream);
      type.push(new FunctionTypeNode(parameters, returnType));
    }
  } while (stream.matchValue("|", true));
  return type;
}

function parseTypeDeclaration(stream) {
  stream.expectValue("type");
  const name = stream.expectType(IdentifierToken);
  const fields = [];
  while (stream.hasMore() && !stream.matchValue("end")) {
    if (stream.matchType(CommentToken)) {
      fields.push(new CommentNode(stream.next()));
    } else {
      fields.push(parseNameAndType(stream));
    }
  }
  stream.expectValue("end");
  return new TypeDeclarationNode(name, fields);
}

function parseNameAndType(stream) {
  const name = stream.expectType(IdentifierToken);
  stream.expectValue(":");
  const type = parseTypeSpecifier(stream);
  return new NameAndTypeNode(name, type);
}

function parseFunction(stream, hasName) {
  stream.expectValue("func");
  const name = hasName ? stream.expectType(IdentifierToken) : null;
  const { parameters, returnType } = parseFunctionSignature(stream);
  const code = [];
  while (stream.hasMore() && !stream.matchValue("end")) {
    code.push(parseStatement(stream));
  }
  stream.expectValue("end");
  return new FunctionNode(name, parameters, returnType, code);
}

function parseFunctionSignature(stream) {
  const parameters = [];
  stream.expectValue("(");
  while (stream.hasMore()) {
    if (stream.matchValue(")")) break;
    parameters.push(parseNameAndType(stream));
    if (stream.matchValue(")")) break;
    stream.expectValue(",");
  }
  stream.expectValue(")");
  const returnType = stream.matchValue(":", true) ? parseTypeSpecifier(stream) : null;
  return { name, parameters, returnType };
}

function parseIf(stream) {
  stream.expectValue("if");
  const condition = parseExpression(stream);
  stream.expectValue("then");

  const trueBlock = [];
  while (stream.hasMore() && !stream.matchValues(["elseif", "else", "end"])) {
    trueBlock.push(parseStatement(stream));
  }

  const elseIfs = [];
  while (stream.hasMore() && stream.matchValue("elseif", true)) {
    const elseIfCondition = parseExpression(stream);
    stream.expectValue("then");
    const elseIfBlock = [];
    while (stream.hasMore() && !stream.matchValues(["elseif", "else", "end"])) {
      elseIfBlock.push(parseStatement(stream));
    }

    elseIfs.push(new IfNode(elseIfCondition, elseIfBlock, [], []));
  }

  const falseBlock = [];
  if (stream.matchValue("else", true)) {
    while (stream.hasMore() && !stream.matchValue("end")) {
      falseBlock.push(parseStatement(stream));
    }
  }

  stream.expectValue("end");

  return new IfNode(condition, trueBlock, elseIfs, falseBlock);
}

function parseWhile(stream) {
  stream.expectValue("while");
  const condition = parseExpression(stream);
  stream.expectValue("do");

  const block = [];
  while (stream.hasMore() && !stream.matchValue("end")) {
    block.push(parseStatement(stream));
  }
  stream.expectValue("end");
  return new WhileNode(condition, block);
}

function parseDo(stream) {
  stream.expectValue("do");
  const block = [];
  while (stream.hasMore() && !stream.matchValue("while")) {
    block.push(parseStatement(stream));
  }
  stream.expectValue("while");
  const condition = parseExpression(stream);
  return new DoNode(condition, block);
}

function parseFor(stream) {
  stream.expectValue("for");

  if (stream.matchValue("each", true)) {
    const identifier = stream.expectType(IdentifierToken);
    stream.expectValue("in");
    const array = parseExpression(stream);
    stream.expectValue("do");
    const block = [];
    while (stream.hasMore() && !stream.matchValue("end")) {
      block.push(parseStatement(stream));
    }
    stream.expectValue("end");
    return new ForEachNode(identifier, array, block);
  } else {
    const identifier = stream.expectType(IdentifierToken);
    stream.expectValue("from");
    const start = parseExpression(stream);
    stream.expectValue("to");
    const end = parseExpression(stream);
    let step = null;
    if (stream.matchValue("step", true)) {
      step = parseExpression(stream);
    }
    stream.expectValue("do");
    const block = [];
    while (stream.hasMore() && !stream.matchValue("end")) {
      block.push(parseStatement(stream));
    }
    stream.expectValue("end");
    return new ForNode(identifier, start, end, step, block);
  }
}

function parseExpression(stream) {
  return parseTernaryOperator(stream);
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

const binaryOperatorPrecedence = [["="], ["|", "&", "^"], ["==", "!="], ["<", "<=", ">", ">="], ["+", "-"], ["/", "*", "%"], ["is"]];

function parseBinaryOperator(stream, level) {
  const nextLevel = level + 1;
  let leftExpression = nextLevel == binaryOperatorPrecedence.length ? parseUnaryOperator(stream) : parseBinaryOperator(stream, nextLevel);
  const operators = binaryOperatorPrecedence[level];
  while (stream.hasMore() && stream.matchValues(operators, false)) {
    const operator = stream.next();
    if (operator.value == "is") {
      const type = parseTypeSpecifier(stream);
      leftExpression = new IsOperatorNode(leftExpression, type);
    } else {
      const rightExpression = nextLevel == binaryOperatorPrecedence.length ? parseUnaryOperator(stream) : parseBinaryOperator(stream, nextLevel);
      leftExpression = new BinaryOperatorNode(leftExpression, operator, rightExpression);
    }
  }
  return leftExpression;
}

const unaryOperators = ["!", "+", "-"];

function parseUnaryOperator(stream) {
  if (stream.matchValues(unaryOperators, false)) {
    return new UnaryOperatorNode(stream.next(), parseExpression(stream));
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

function parseAccessOrCallOrLiteral(stream) {
  if (stream.matchValue("{", true)) {
    const keyValues = [];
    while (stream.hasMore()) {
      if (stream.matchValue("}")) break;
      const key = new StringLiteralNode(stream.expectType(StringToken));
      stream.expectValue(":");
      const value = parseExpression(stream);
      keyValues.push(key);
      keyValues.push(value);
      if (stream.matchValue("}")) break;
      stream.expectValue(",");
    }
    stream.expectValue("}");
    return new MapLiteralNode(keyValues);
  } else if (stream.matchValue("[", true)) {
    const elements = [];
    while (stream.hasMore()) {
      if (stream.matchValue("]")) break;
      elements.push(parseExpression(stream));
      if (stream.matchValue("]")) break;
      stream.expectValue(",");
    }
    stream.expectValue("]");
    return new ArrayLiteralNode(elements);
  } else if (stream.matchValue("func")) {
    return parseFunction(stream, false);
  } else if (stream.matchType(StringToken)) {
    return new StringLiteralNode(stream.next());
  } else if (stream.matchType(NumberToken)) {
    return new NumberLiteralNode(stream.next());
  } else if (stream.matchValues(["true", "false"])) {
    return new BooleanLiteralNode(stream.next());
  } else if (stream.matchType(IdentifierToken)) {
    return parseAccessOrCall(stream);
  } else if (stream.matchType(NothingToken)) {
    return new NothingLiteralNode(stream.next());
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

import { LittleFootError } from "./error";
import { tokenize, TokenStream, IdentifierToken, StringToken, NumberToken, CommentToken, NothingToken } from "./tokenizer";
// prettier-ignore
import { ArrayLiteralNode, ArrayTypeNode, AstNode, BinaryOperatorNode, BooleanLiteralNode, CommentNode, DoNode, ExpressionNode, ForEachNode, ForNode, FunctionCallNode, FunctionLiteralNode, FunctionNode, FunctionTypeNode, IfNode, IsOperatorNode, MapLiteralNode, MapOrArrayAccessNode, MapTypeNode, MemberAccessNode, MethodCallNode, NameAndTypeNode, NothingLiteralNode, NumberLiteralNode, PlainTypeNode, StatementNode, StringLiteralNode, TernaryOperatorNode, RecordNode as RecordNode, TypeSpecifierNode, UnaryOperatorNode, VariableAccessNode, VariableNode, WhileNode } from "./ast";
import { Source } from "./source";

export function parse(source: Source) {
  let ast: AstNode[] = [];
  const { tokens, errors } = tokenize(source);
  if (errors.length > 0) return { ast, errors };

  const stream = new TokenStream(source, tokens);

  try {
    while (stream.hasMore()) {
      if (stream.matchValue("func")) {
        ast.push(parseFunction(stream, true) as FunctionNode);
      } else if (stream.matchValue("record")) {
        ast.push(parseRecord(stream));
      } else {
        ast.push(parseStatement(stream));
      }
    }
  } catch (e) {
    if (e instanceof LittleFootError) errors.push(e);
    else errors.push(new LittleFootError(0, 1, source, "Internal error: " + (e as any).message + "\n" + (e as any).stack));
  } finally {
    return { ast, errors };
  }
}

function parseStatement(stream: TokenStream): StatementNode {
  if (stream.matchValue("var")) {
    return parseVariable(stream);
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

function parseVariable(stream: TokenStream) {
  const firstToken = stream.expectValue("var");
  const identifier = stream.expectType(IdentifierToken);
  const type = stream.matchValue(":", true) ? parseTypeSpecifier(stream) : null;
  stream.expectValue("=");
  const initializer = parseExpression(stream);
  return new VariableNode(firstToken, identifier, type, initializer);
}

function parseTypeSpecifier(stream: TokenStream) {
  const type: TypeSpecifierNode[] = [];
  do {
    if (stream.matchType(IdentifierToken) || stream.matchType(NothingToken)) {
      type.push(new PlainTypeNode(stream.next()));
    } else if (stream.matchValue("[")) {
      type.push(new ArrayTypeNode(stream.expectValue("["), parseTypeSpecifier(stream), stream.expectValue("]")));
    } else if (stream.matchValue("{")) {
      type.push(new MapTypeNode(stream.expectValue("{"), parseTypeSpecifier(stream), stream.expectValue("}")));
    } else if (stream.matchValue("(")) {
      const { openingParanthesis, parameters, closingParanthesis, returnType } = parseFunctionSignature(stream);
      type.push(
        new FunctionTypeNode(
          openingParanthesis,
          parameters,
          returnType,
          returnType ? returnType[returnType.length - 1].lastToken : closingParanthesis
        )
      );
    } else {
      if (stream.hasMore()) {
        const token = stream.next();
        throw new LittleFootError(token.start, token.end, stream.source, `Expected a type specifier, but got ${token.value}.`);
      } else {
        throw new LittleFootError(
          stream.source.text.length,
          stream.source.text.length,
          stream.source,
          "Expected a type specifier, but reached end of file."
        );
      }
    }
  } while (stream.matchValue("|", true));
  return type;
}

function parseRecord(stream: TokenStream) {
  const firstToken = stream.expectValue("record");
  const name = stream.expectType(IdentifierToken);
  const fields = [];
  while (stream.hasMore() && !stream.matchValue("end")) {
    if (stream.matchType(CommentToken)) {
      fields.push(new CommentNode([stream.next<CommentToken>()]));
    } else {
      fields.push(parseNameAndType(stream));
    }
  }
  const lastToken = stream.expectValue("end");
  return new RecordNode(firstToken, name, fields, lastToken);
}

function parseNameAndType(stream: TokenStream) {
  const name = stream.expectType(IdentifierToken);
  stream.expectValue(":");
  const type = parseTypeSpecifier(stream);
  return new NameAndTypeNode(name, type);
}

function parseFunction(stream: TokenStream, hasName: boolean) {
  const firstToken = stream.expectValue("func");
  const name = hasName ? stream.expectType(IdentifierToken) : null;
  const { parameters, returnType } = parseFunctionSignature(stream);
  const code = [];
  while (stream.hasMore() && !stream.matchValue("end")) {
    code.push(parseStatement(stream));
  }
  const lastToken = stream.expectValue("end");
  if (hasName) return new FunctionNode(firstToken, name, parameters, returnType, code, lastToken);
  else return new FunctionLiteralNode(firstToken, parameters, returnType, code, lastToken);
}

function parseFunctionSignature(stream: TokenStream) {
  const parameters = [];
  const openingParanthesis = stream.expectValue("(");
  while (stream.hasMore()) {
    if (stream.matchValue(")")) break;
    parameters.push(parseNameAndType(stream));
    if (stream.matchValue(")")) break;
    stream.expectValue(",");
  }
  const closingParanthesis = stream.expectValue(")");
  const returnType = stream.matchValue(":", true) ? parseTypeSpecifier(stream) : null;
  return { openingParanthesis, parameters, closingParanthesis, returnType };
}

function parseIf(stream: TokenStream) {
  const firstToken = stream.expectValue("if");
  const condition = parseExpression(stream);
  stream.expectValue("then");

  const trueBlock = [];
  while (stream.hasMore() && !stream.matchValues(["elseif", "else", "end"])) {
    trueBlock.push(parseStatement(stream));
  }

  const elseIfs = [];
  while (stream.hasMore() && stream.matchValue("elseif")) {
    const firstElseIfToken = stream.expectValue("elseif");
    const elseIfCondition = parseExpression(stream);
    stream.expectValue("then");
    const elseIfBlock = [];
    while (stream.hasMore() && !stream.matchValues(["elseif", "else", "end"])) {
      elseIfBlock.push(parseStatement(stream));
    }

    elseIfs.push(
      new IfNode(
        firstElseIfToken,
        elseIfCondition,
        elseIfBlock,
        [],
        [],
        elseIfBlock.length > 0 ? elseIfBlock[elseIfBlock.length - 1].lastToken : elseIfCondition.lastToken
      )
    );
  }

  const falseBlock = [];
  if (stream.matchValue("else", true)) {
    while (stream.hasMore() && !stream.matchValue("end")) {
      falseBlock.push(parseStatement(stream));
    }
  }

  const lastToken = stream.expectValue("end");

  return new IfNode(firstToken, condition, trueBlock, elseIfs, falseBlock, lastToken);
}

function parseWhile(stream: TokenStream) {
  const firstToken = stream.expectValue("while");
  const condition = parseExpression(stream);
  stream.expectValue("do");

  const block = [];
  while (stream.hasMore() && !stream.matchValue("end")) {
    block.push(parseStatement(stream));
  }
  const lastToken = stream.expectValue("end");
  return new WhileNode(firstToken, condition, block, lastToken);
}

function parseDo(stream: TokenStream) {
  const firstToken = stream.expectValue("do");
  const block = [];
  while (stream.hasMore() && !stream.matchValue("while")) {
    block.push(parseStatement(stream));
  }
  stream.expectValue("while");
  const condition = parseExpression(stream);
  return new DoNode(firstToken, condition, block);
}

function parseFor(stream: TokenStream) {
  const firstToken = stream.expectValue("for");

  if (stream.matchValue("each", true)) {
    const identifier = stream.expectType(IdentifierToken);
    stream.expectValue("in");
    const array = parseExpression(stream);
    stream.expectValue("do");
    const block = [];
    while (stream.hasMore() && !stream.matchValue("end")) {
      block.push(parseStatement(stream));
    }
    const lastToken = stream.expectValue("end");
    return new ForEachNode(firstToken, identifier, array, block, lastToken);
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
    const lastToken = stream.expectValue("end");
    return new ForNode(firstToken, identifier, start, end, step, block, lastToken);
  }
}

function parseExpression(stream: TokenStream): ExpressionNode {
  return parseTernaryOperator(stream);
}

function parseTernaryOperator(stream: TokenStream): ExpressionNode {
  const condition = parseBinaryOperator(stream, 0);
  if (stream.matchValue("?", true)) {
    const trueExpression = parseTernaryOperator(stream);
    stream.expectValue(":");
    const falseExpression = parseTernaryOperator(stream);
    return new TernaryOperatorNode(condition, trueExpression, falseExpression);
  } else {
    return condition;
  }
}

const binaryOperatorPrecedence = [["="], ["|", "&", "^"], ["==", "!="], ["<", "<=", ">", ">="], ["+", "-"], ["/", "*", "%"], ["is"]];

function parseBinaryOperator(stream: TokenStream, level: number): ExpressionNode {
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

function parseUnaryOperator(stream: TokenStream) {
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

function parseAccessOrCallOrLiteral(stream: TokenStream) {
  if (stream.matchValue("{")) {
    const firstToken = stream.expectValue("{");
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
    const lastToken = stream.expectValue("}");
    return new MapLiteralNode(firstToken, keyValues, lastToken);
  } else if (stream.matchValue("[")) {
    const firstToken = stream.expectValue("[");
    const elements = [];
    while (stream.hasMore()) {
      if (stream.matchValue("]")) break;
      elements.push(parseExpression(stream));
      if (stream.matchValue("]")) break;
      stream.expectValue(",");
    }
    const lastToken = stream.expectValue("]");
    return new ArrayLiteralNode(firstToken, elements, lastToken);
  } else if (stream.matchValue("func")) {
    return parseFunction(stream, false) as FunctionLiteralNode;
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

function parseAccessOrCall(stream: TokenStream) {
  let result: ExpressionNode = new VariableAccessNode(stream.expectType(IdentifierToken));
  while (stream.hasMore() && stream.matchValues(["(", "[", "."])) {
    if (stream.matchValue("(")) {
      const openingParanthesis = stream.expectValue("(");
      const args = parseArguments(stream);
      const closingParanthesis = stream.expectValue(")");
      if (result instanceof VariableAccessNode || result instanceof MapOrArrayAccessNode) {
        result = new FunctionCallNode(result, args, closingParanthesis);
      } else if (result instanceof MemberAccessNode) {
        result = new MethodCallNode(result, args, closingParanthesis);
      } else {
        throw new LittleFootError(openingParanthesis.start, closingParanthesis.end, stream.source, `Expected a variable, field, or method.`);
      }
    } else if (stream.matchValue("[")) {
      const openingBracket = stream.expectValue("[");
      const keyOrIndex = parseExpression(stream);
      const lastToken = stream.expectValue("]");
      result = new MapOrArrayAccessNode(result, keyOrIndex, lastToken);
    } else if (stream.matchValue(".", true)) {
      const identifier = stream.expectType(IdentifierToken);
      result = new MemberAccessNode(result, identifier);
    }
  }
  return result;
}

function parseArguments(stream: TokenStream) {
  const args = [];
  while (stream.hasMore() && !stream.matchValue(")")) {
    args.push(parseExpression(stream));
    if (!stream.matchValue(")")) stream.expectValue(",");
  }
  return args;
}

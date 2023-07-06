import { LittleFootError } from "./error.js";
import { tokenize, TokenStream, IdentifierToken, StringToken, NumberToken, CommentToken, NothingToken } from "./tokenizer.js";

export class AstNode {}

export class CommentNode extends AstNode {
  constructor(lines) {
    super();
    this.nodeType = "comment";
    this.lines = lines;
  }
}

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
  const type = stream.matchValue(":", true) ? parseTypeSpecifier(stream) : null;
  stream.expectValue("=");
  const initializer = parseExpression(stream);
  return new VariableNode(identifier, initializer, type);
}

export class PlainTypeNode extends AstNode {
  constructor(typeName) {
    super();
    this.nodeType = "plain type";
    this.typeName = typeName;
  }
}

export class ArrayTypeNode extends AstNode {
  constructor(elementTypes) {
    super();
    this.nodeType = "array type";
    this.elementTypes = elementTypes;
  }
}

export class MapTypeNode extends AstNode {
  constructor(valueTypes) {
    super();
    this.nodeType = "map type";
    this.valueTypes = valueTypes;
  }
}

export class FunctionTypeNode extends AstNode {
  constructor(parameters, returnType) {
    super();
    this.nodeType = "function type";
    this.parameters = parameters;
    this.returnType = returnType;
  }
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

export class TypeDeclarationNode extends AstNode {
  constructor(name, fields) {
    super();
    this.nodeType = "type declaration";
    this.name = name;
    this.fields = fields;
  }
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

export class NameAndTypeNode extends AstNode {
  constructor(name, type) {
    super();
    this.nodeType = "name and type";
    this.name = name;
    this.type = type;
  }
}

function parseNameAndType(stream) {
  const name = stream.expectType(IdentifierToken);
  stream.expectValue(":");
  const type = parseTypeSpecifier(stream);
  return new NameAndTypeNode(name, type);
}

export class FunctionNode extends AstNode {
  constructor(name, parameters, returnType, code) {
    super();
    this.nodeType = "function declaration";
    this.name = name;
    this.parameters = parameters;
    this.returnType = returnType;
    this.code = code;
  }
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

export class IfNode extends AstNode {
  constructor(condition, trueBlock, elseIfs, falseBlock) {
    super();
    this.nodeType = "if";
    this.condition = condition;
    this.trueBlock = trueBlock;
    this.elseIfs = elseIfs;
    this.falseBlock = falseBlock;
  }
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

export class WhileNode extends AstNode {
  constructor(condition, block) {
    super();
    this.nodeType = "while";
    this.condition = condition;
    this.block = block;
  }
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

export class DoNode extends AstNode {
  constructor(condition, block) {
    super();
    this.nodeType = "do";
    this.condition = condition;
    this.block = block;
  }
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

export class ForNode extends AstNode {
  constructor(identifier, start, end, step, block) {
    super();
    this.nodeType = "for";
    this.identifier = identifier;
    this.start = start;
    this.end = end;
    this.step = step;
    this.block = block;
  }
}

export class ForEachNode extends AstNode {
  constructor(identifier, array, block) {
    super();
    this.nodeType = "for each";
    this.identifier = identifier;
    this.array = array;
    this.block = block;
  }
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

const binaryOperatorPrecedence = [["="], ["|", "&", "^"], ["==", "!="], ["<", "<=", ">", ">="], ["+", "-"], ["/", "*", "%"], ["is"]];

export class BinaryOperatorNode extends AstNode {
  constructor(leftExpression, operator, rightExpression) {
    super();
    this.nodeType = "binary operator";
    this.leftExpression = leftExpression;
    this.operator = operator;
    this.rightExpression = rightExpression;
  }
}

export class IsOperatorNode extends AstNode {
  constructor(leftExpression, type) {
    super();
    this.nodeType = "is operator";
    this.leftExpression = leftExpression;
    this.type = type;
  }
}

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

export class UnaryOperatorNode extends AstNode {
  constructor(operator, expression) {
    super();
    this.nodeType = "unary operator";
    this.operator = operator;
    this.expression = expression;
  }
}

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

export class NothingLiteralNode extends AstNode {
  constructor(token) {
    super();
    this.nodeType = "nothing";
    this.token = token;
  }
}

export class ArrayLiteralNode extends AstNode {
  constructor(elements) {
    super();
    this.nodeType = "array literal";
    this.elements = elements;
  }
}

export class MapLiteralNode extends AstNode {
  constructor(keyValues) {
    super();
    this.nodeType = "map literal";
    this.keyValues = keyValues;
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

import { LittleFootError } from "./error";
// prettier-ignore
import { tokenize, TokenStream, IdentifierToken, StringToken, NumberToken, NothingToken, RecordOpeningToken, OperatorToken } from "./tokenizer";
// prettier-ignore
import { ListLiteralNode, ListTypeNode, BinaryOperatorNode, BooleanLiteralNode, DoNode, ExpressionNode, ForEachNode, ForNode, FunctionCallNode, FunctionLiteralNode, FunctionNode, FunctionTypeNode, IfNode, IsOperatorNode, MapLiteralNode, MapOrListAccessNode, MapTypeNode, MemberAccessNode, MethodCallNode, NameAndTypeNode, NothingLiteralNode, NumberLiteralNode, StatementNode, StringLiteralNode, TernaryOperatorNode, TypeSpecifierNode, UnaryOperatorNode, VariableAccessNode, VariableNode, WhileNode, RecordTypeNode, RecordLiteralNode, ContinueNode, BreakNode, ReturnNode, TypeNode, TypeReferenceNode as TypeNameNode, AstNode, MixinTypeNode, UnionTypeNode, ImportNode, ImportedNameNode, LoopVariable, AsOperatorNode } from "./ast";
import { Source, SourceLocation } from "./source";

export enum Attribute {
  Export,
  External,
}

export function parse(source: Source, errors: LittleFootError[]) {
  const ast: AstNode[] = [];
  const tokens = tokenize(source, errors);
  if (errors.length > 0) return ast;

  const stream = new TokenStream(source, tokens);

  try {
    while (stream.matchValue("import")) {
      ast.push(parseImport(stream));
    }

    while (stream.hasMore()) {
      const attributes = parseAttributes(stream);

      if (stream.matchValue("func") || stream.matchValue("operator")) {
        ast.push(parseFunction(stream, true, attributes));
      } else if (stream.matchValue("type")) {
        ast.push(parseType(stream, attributes));
      } else if (stream.matchValue("var")) {
        ast.push(parseVariable(stream, attributes));
      } else {
        ast.push(parseStatement(stream));
      }
    }
  } catch (e) {
    if (e instanceof LittleFootError) errors.push(e);
    else errors.push(new LittleFootError(new SourceLocation(source, 0, 1), "Internal error: " + (e as any).message + "\n" + (e as any).stack));
  } finally {
    return ast;
  }
}

const attributeValues = ["export", "external"];

function parseAttributes(stream: TokenStream) {
  const attributes = [];
  while (stream.matchValues(attributeValues)) {
    const attributeValue = stream.next();
    switch (attributeValue.value) {
      case "external": {
        attributes.push(Attribute.External);
        break;
      }
      case "export": {
        attributes.push(Attribute.Export);
        break;
      }
      default: {
        throw new LittleFootError(attributeValue.location, `Expected export or external, but got '${attributeValue.value}`);
      }
    }
  }
  return attributes;
}

function parseImport(stream: TokenStream): ImportNode {
  const firstToken = stream.expectValue("import");
  const importedNames: ImportedNameNode[] = [];

  if (stream.matchType(StringToken)) {
  } else {
    while (stream.matchType(IdentifierToken)) {
      const name = stream.expectType(IdentifierToken);
      const alias = stream.matchValue("as", true) ? stream.expectType(IdentifierToken) : null;
      importedNames.push(new ImportedNameNode(name, alias));
      stream.matchValue(",", true);
    }
    stream.expectValue("from");
  }
  const path = stream.expectType(StringToken);
  return new ImportNode(firstToken, importedNames, path);
}

function parseStatement(stream: TokenStream): StatementNode {
  if (stream.matchValue("var") || stream.matchValue("const")) {
    return parseVariable(stream);
  } else if (stream.matchValue("if")) {
    return parseIf(stream);
  } else if (stream.matchValue("while")) {
    return parseWhile(stream);
  } else if (stream.matchValue("do")) {
    return parseDo(stream);
  } else if (stream.matchValue("for")) {
    return parseFor(stream);
  } else if (stream.matchValue("continue")) {
    return new ContinueNode(stream.expectValue("continue"));
  } else if (stream.matchValue("break")) {
    return new BreakNode(stream.expectValue("break"));
  } else if (stream.matchValue("return")) {
    const firstToken = stream.expectValue("return");
    return new ReturnNode(firstToken, stream.matchValue(";", true) ? null : parseExpression(stream));
  } else {
    return parseExpression(stream);
  }
}

function parseVariable(stream: TokenStream, attributes: Attribute[] = []) {
  const firstToken = stream.matchValue("var") ? stream.expectValue("var") : stream.expectValue("const");
  const identifier = stream.expectType(IdentifierToken);
  if (attributes.length > 1) throw new LittleFootError(identifier.location, `Expected only 'export' attribute but got '${attributes.join(",")}'`);
  if (attributes.length == 1 && attributes[0] != Attribute.Export)
    throw new LittleFootError(identifier.location, `Expected 'export' attribute but got '${attributes.join(",")}'`);
  const type = stream.matchValue(":", true) ? parseTypeSpecifier(stream) : null;
  stream.expectValue("=");
  const initializer = parseExpression(stream);
  return new VariableNode(
    firstToken,
    identifier,
    type,
    initializer,
    attributes.length == 1 && attributes[0] == Attribute.Export,
    firstToken.value == "const"
  );
}

function parseType(stream: TokenStream, attributes: Attribute[] = []) {
  const firstToken = stream.expectValue("type");
  const name = stream.expectType(IdentifierToken);
  if (attributes.length > 1) throw new LittleFootError(name.location, `Expected only 'export' attribute but got '${attributes.join(",")}'`);
  if (attributes.length == 1 && attributes[0] != Attribute.Export)
    throw new LittleFootError(name.location, `Expected 'export' attribute but got '${attributes.join(",")}'`);
  stream.expectValue("=");
  const type = parseTypeSpecifier(stream);
  return new TypeNode(firstToken, name, type, attributes.length == 1 && attributes[0] == Attribute.Export);
}

function parseTypeSpecifier(stream: TokenStream) {
  const types: TypeSpecifierNode[] = [];
  const operators: OperatorToken[] = [];
  do {
    if (stream.matchType(IdentifierToken) || stream.matchType(NothingToken)) {
      types.push(new TypeNameNode(stream.next()));
    } else if (stream.matchValue("[")) {
      types.push(new ListTypeNode(stream.expectValue("["), parseTypeSpecifier(stream), stream.expectValue("]")));
    } else if (stream.matchValue("{")) {
      types.push(new MapTypeNode(stream.expectValue("{"), parseTypeSpecifier(stream), stream.expectValue("}")));
    } else if (stream.matchType(RecordOpeningToken)) {
      const firstToken = stream.expectType(RecordOpeningToken);
      const fields = [];
      while (stream.hasMore() && !stream.matchValue(">")) {
        fields.push(parseNameAndType(stream));
        if (!stream.matchValue(">")) stream.expectValue(",");
      }
      const lastToken = stream.expectValue(">");
      types.push(new RecordTypeNode(firstToken, fields, lastToken));
    } else if (stream.matchValue("(")) {
      const { openingParanthesis, parameters, closingParanthesis, returnType } = parseFunctionSignature(stream);
      types.push(new FunctionTypeNode(openingParanthesis, parameters, returnType, returnType ? returnType.location : closingParanthesis.location));
    } else {
      if (stream.hasMore()) {
        const token = stream.next();
        throw new LittleFootError(token.location, `Expected a type specifier, but got ${token.value}.`);
      } else {
        throw new LittleFootError(
          new SourceLocation(stream.source, stream.source.text.length, stream.source.text.length),
          "Expected a type specifier, but reached end of file."
        );
      }
    }
    if (stream.matchValue("|") || stream.matchValue("+")) {
      operators.push(stream.next());
    } else {
      break;
    }
  } while (true);

  // Only one type given, return it directly
  if (types.length == 1) {
    return types[0];
  }

  // Otherwise coalesc mixin types
  const coalescedTypes: TypeSpecifierNode[] = [];

  coalescedTypes.push(types[0]);
  for (let i = 1; i < types.length; i++) {
    const type = types[i];
    const operator = operators[i - 1];
    if (operator.value == "+") {
      const lastType = coalescedTypes[coalescedTypes.length - 1];
      if (lastType instanceof MixinTypeNode) {
        lastType.mixinTypes.push(type);
      } else {
        coalescedTypes[coalescedTypes.length - 1] = new MixinTypeNode([lastType, type]);
      }
    } else {
      coalescedTypes.push(type);
    }
  }

  if (coalescedTypes.length == 1) {
    return coalescedTypes[0];
  } else {
    return new UnionTypeNode(coalescedTypes);
  }
}

function parseNameAndType(stream: TokenStream) {
  const name = stream.expectType(IdentifierToken);
  stream.expectValue(":");
  const type = parseTypeSpecifier(stream);
  return new NameAndTypeNode(name, type);
}

function parseFunction(stream: TokenStream, hasName: boolean, attributes: Attribute[] = []) {
  const firstToken = stream.matchValue("func") ? stream.expectValue("func") : stream.expectValue("operator");
  const isOperator = firstToken.value == "operator";
  const name = hasName ? (isOperator ? stream.expectType(OperatorToken) : stream.expectType(IdentifierToken)) : null;

  if (hasName == false && attributes.length > 0)
    throw new LittleFootError(firstToken.location, "Function literals can not have attributes like export or external.");
  if (hasName && attributes.length > 3)
    throw new LittleFootError(name!.location, "Function can only have the attribute export, external, or export and external.");
  if (hasName && attributes.length == 1 && !(attributes[0] == Attribute.Export || attributes[0] == Attribute.External))
    throw new LittleFootError(name!.location, "Function can only have the attribute export, external, or export and external.");
  if (
    hasName &&
    attributes.length == 2 &&
    !(
      (attributes[0] == Attribute.Export && attributes[1] == Attribute.External) ||
      (attributes[0] == Attribute.External && attributes[1] == Attribute.Export)
    )
  )
    throw new LittleFootError(name!.location, "Function can only have the attribute export, external, or export and external.");

  const { parameters, returnType, closingParanthesis } = parseFunctionSignature(stream);
  const code = [];
  let lastLocation: SourceLocation;
  const exported = attributes.find((attribute) => attribute == Attribute.Export);
  const external = attributes.find((attribute) => attribute == Attribute.External);
  if (!external) {
    while (stream.hasMore() && !stream.matchValue("end")) {
      code.push(parseStatement(stream));
    }
    lastLocation = stream.expectValue("end").location;
  } else {
    lastLocation = stream.expectValue(";").location;
  }
  if (hasName) {
    return new FunctionNode(firstToken, name!, parameters, returnType, code, exported !== undefined, external !== undefined, lastLocation);
  } else {
    return new FunctionLiteralNode(firstToken, parameters, returnType, code, lastLocation);
  }
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
  if (stream.matchValue(":", true)) {
    const returnType = parseTypeSpecifier(stream);
    return { openingParanthesis, parameters, closingParanthesis, returnType };
  } else {
    return { openingParanthesis, parameters, closingParanthesis, returnType: null };
  }
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
        elseIfBlock.length > 0 ? elseIfBlock[elseIfBlock.length - 1].location : elseIfCondition.location
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

  return new IfNode(firstToken, condition, trueBlock, elseIfs, falseBlock, lastToken.location);
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
    const loopVariable = stream.expectType(IdentifierToken);
    stream.expectValue("in");
    const list = parseExpression(stream);
    stream.expectValue("do");
    const block = [];
    while (stream.hasMore() && !stream.matchValue("end")) {
      block.push(parseStatement(stream));
    }
    const lastToken = stream.expectValue("end");
    return new ForEachNode(firstToken, new LoopVariable(loopVariable), list, block, lastToken);
  } else {
    const loopVariable = stream.expectType(IdentifierToken);
    stream.expectValue("from");
    const from = parseExpression(stream);
    stream.expectValue("to");
    const to = parseExpression(stream);
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
    return new ForNode(firstToken, new LoopVariable(loopVariable), from, to, step, block, lastToken);
  }
}

export type ExpressionContext = { inRecord: boolean; inParans: boolean };

function parseExpression(stream: TokenStream, context: ExpressionContext = { inRecord: false, inParans: false }): ExpressionNode {
  return parseTernaryOperator(stream, context);
}

function parseTernaryOperator(stream: TokenStream, context: ExpressionContext): ExpressionNode {
  const condition = parseBinaryOperator(stream, 0, context);
  if (stream.matchValue("?", true)) {
    const trueExpression = parseTernaryOperator(stream, context);
    stream.expectValue(":");
    const falseExpression = parseTernaryOperator(stream, context);
    return new TernaryOperatorNode(condition, trueExpression, falseExpression);
  } else {
    return condition;
  }
}

const binaryOperatorPrecedence = [["="], ["or", "and", "xor"], ["==", "!="], ["<", "<=", ">", ">="], ["+", "-"], ["/", "*", "%"], ["is"], ["as"]];

function parseBinaryOperator(stream: TokenStream, level: number, context: ExpressionContext): ExpressionNode {
  const nextLevel = level + 1;
  let leftExpression =
    nextLevel == binaryOperatorPrecedence.length ? parseUnaryOperator(stream, context) : parseBinaryOperator(stream, nextLevel, context);
  let operators = binaryOperatorPrecedence[level];
  if (context.inRecord && !context.inParans) operators = operators.filter((op) => op != ">");
  while (stream.hasMore() && stream.matchValues(operators)) {
    const operator = stream.next();
    if (operator.value == "is") {
      const type = parseTypeSpecifier(stream);
      leftExpression = new IsOperatorNode(leftExpression, type);
    } else if (operator.value == "as") {
      const type = parseTypeSpecifier(stream);
      leftExpression = new AsOperatorNode(leftExpression, type);
    } else {
      const rightExpression =
        nextLevel == binaryOperatorPrecedence.length ? parseUnaryOperator(stream, context) : parseBinaryOperator(stream, nextLevel, context);
      leftExpression = new BinaryOperatorNode(leftExpression, operator, rightExpression);
    }
  }
  return leftExpression;
}

const unaryOperators = ["not", "+", "-"];

function parseUnaryOperator(stream: TokenStream, context: ExpressionContext) {
  if (stream.matchValues(unaryOperators)) {
    return new UnaryOperatorNode(stream.next(), parseExpression(stream, context));
  } else {
    if (stream.matchValue("(", true)) {
      context.inParans = true;
      const expression = parseExpression(stream, context);
      stream.expectValue(")");
      context.inParans = false;
      return expression;
    } else {
      return parseAccessOrCallOrLiteral(stream);
    }
  }
}

function parseAccessOrCallOrLiteral(stream: TokenStream) {
  if (stream.matchValue("{")) {
    const firstToken = stream.expectValue("{");

    // Empty map literals with type specifier
    if (stream.matchValue(":", true)) {
      const typeNode = parseTypeSpecifier(stream);
      const lastToken = stream.expectValue("}");
      return new MapLiteralNode(firstToken, [], [], typeNode, lastToken);
    } else {
      const keys: StringToken[] = [];
      const values: ExpressionNode[] = [];
      while (stream.hasMore()) {
        if (stream.matchValue("}")) break;
        const key = stream.expectType(StringToken);
        stream.expectValue(":");
        const value = parseExpression(stream);
        keys.push(key);
        values.push(value);
        if (stream.matchValue("}")) break;
        stream.expectValue(",");
      }
      const lastToken = stream.expectValue("}");
      return new MapLiteralNode(firstToken, keys, values, null, lastToken);
    }
  } else if (stream.matchValue("[")) {
    const firstToken = stream.expectValue("[");

    // Empty list literal with type specifier
    if (stream.matchValue(":", true)) {
      const typeNode = parseTypeSpecifier(stream);
      const lastToken = stream.expectValue("]");
      return new ListLiteralNode(firstToken, [], typeNode, lastToken);
    } else {
      const elements: ExpressionNode[] = [];
      while (stream.hasMore()) {
        if (stream.matchValue("]")) break;
        elements.push(parseExpression(stream));
        if (stream.matchValue("]")) break;
        stream.expectValue(",");
      }
      const lastToken = stream.expectValue("]");
      return new ListLiteralNode(firstToken, elements, null, lastToken);
    }
  } else if (stream.matchType(RecordOpeningToken)) {
    const firstToken = stream.expectType(RecordOpeningToken);
    const fieldNames: IdentifierToken[] = [];
    const fieldValues: ExpressionNode[] = [];
    while (stream.hasMore()) {
      const name = stream.expectType(IdentifierToken);
      stream.expectValue(":");
      const value = parseExpression(stream, { inRecord: true, inParans: false });
      fieldNames.push(name);
      fieldValues.push(value);
      if (stream.matchValue(">")) break;
      stream.expectValue(",");
    }
    const lastToken = stream.expectValue(">");
    return new RecordLiteralNode(firstToken, fieldNames, fieldValues, lastToken);
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
        token.location,
        `Expected a string, number, boolean, variable, field, map, list, function or method call, but got ${token.value}.`
      );
    } else {
      throw new LittleFootError(
        new SourceLocation(stream.source, stream.source.text.length, stream.source.text.length),
        "Expected a string, number, boolean, variable, field, map, list, function or method call, but reached end of file."
      );
    }
  }
}

function parseAccessOrCall(stream: TokenStream) {
  let result: ExpressionNode = new VariableAccessNode(stream.expectType(IdentifierToken));
  while (stream.hasMore() && stream.matchValues(["(", "[", ".", ";"])) {
    if (stream.matchValue("(")) {
      const openingParanthesis = stream.expectValue("(");
      const args = parseArguments(stream);
      const closingParanthesis = stream.expectValue(")");
      if (result instanceof VariableAccessNode || result instanceof MapOrListAccessNode) {
        result = new FunctionCallNode(result, args, closingParanthesis);
      } else if (result instanceof MemberAccessNode) {
        result = new MethodCallNode(result, args, closingParanthesis);
      } else {
        throw new LittleFootError(
          SourceLocation.from(openingParanthesis.location, closingParanthesis.location),
          `Expected a variable, field, or method.`
        );
      }
    } else if (stream.matchValue("[")) {
      const openingBracket = stream.expectValue("[");
      const keyOrIndex = parseExpression(stream);
      const lastToken = stream.expectValue("]");
      result = new MapOrListAccessNode(openingBracket, result, keyOrIndex, lastToken);
    } else if (stream.matchValue(".", true)) {
      const identifier = stream.expectType(IdentifierToken);
      result = new MemberAccessNode(result, identifier);
    } else if (stream.matchValue(";", true)) {
      break;
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

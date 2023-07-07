import { BoolToken, IdentifierToken, NothingToken, NumberToken, OperatorToken, StringToken, Token } from "./tokenizer";

export abstract class AstNode {
  constructor(public readonly nodeLabel: string, public readonly firstToken: Token, public readonly lastToken: Token) {}

  get start(): number {
    return this.firstToken.start;
  }

  get end(): number {
    return this.lastToken.end;
  }
}

export type TypeSpecifierNode = PlainTypeNode | ArrayTypeNode | MapTypeNode | FunctionTypeNode | TupleTypeNode;

export class PlainTypeNode extends AstNode {
  constructor(public readonly typeName: IdentifierToken) {
    super("plain type", typeName, typeName);
  }
}

export class ArrayTypeNode extends AstNode {
  constructor(openingBracket: OperatorToken, public readonly elementTypes: TypeSpecifierNode[], closingBracket: OperatorToken) {
    super("array type", openingBracket, closingBracket);
  }
}

export class MapTypeNode extends AstNode {
  constructor(openingCurly: OperatorToken, public readonly valueTypes: TypeSpecifierNode[], closingCurly: OperatorToken) {
    super("map type", openingCurly, closingCurly);
  }
}

export class FunctionTypeNode extends AstNode {
  constructor(firstToken: Token, public readonly parameters: NameAndTypeNode[], public returnType: TypeSpecifierNode[] | null, lastToken: Token) {
    super("function type", firstToken, lastToken);
  }
}

export class TupleTypeNode extends AstNode {
  constructor(firstToken: Token, public readonly fields: NameAndTypeNode[], lastToken: Token) {
    super("tuple type", firstToken, lastToken);
  }
}

export class NameAndTypeNode extends AstNode {
  constructor(public readonly name: IdentifierToken, public readonly type: TypeSpecifierNode[]) {
    super("name and type", name, type[type.length - 1].lastToken);
  }
}

export class RecordNode extends AstNode {
  constructor(firstToken: Token, public readonly name: IdentifierToken, public readonly fields: NameAndTypeNode[], lastToken: Token) {
    super("type declaration", firstToken, lastToken);
  }
}

export class FunctionNode extends AstNode {
  constructor(
    firstToken: Token,
    public readonly name: IdentifierToken | null,
    public readonly parameters: NameAndTypeNode[],
    public returnType: TypeSpecifierNode[] | null,
    public readonly code: StatementNode[],
    lastToken: Token
  ) {
    super("function declaration", firstToken, lastToken);
  }
}

export type StatementNode = VariableNode | RecordNode | IfNode | WhileNode | ForEachNode | ForNode | DoNode | ExpressionNode;

export class VariableNode extends AstNode {
  constructor(
    firstToken: Token,
    public readonly identifier: IdentifierToken,
    public type: TypeSpecifierNode[] | null,
    public readonly initializer: ExpressionNode
  ) {
    super("variable declaration", firstToken, initializer.lastToken);
  }
}

export class IfNode extends AstNode {
  constructor(
    firstToken: Token,
    public readonly condition: ExpressionNode,
    public readonly trueBlock: StatementNode[],
    public readonly elseIfs: IfNode[],
    public readonly falseBlock: StatementNode[],
    lastToken: Token
  ) {
    super("if", firstToken, lastToken);
  }
}

export class WhileNode extends AstNode {
  constructor(firstToken: Token, public readonly condition: ExpressionNode, public readonly block: StatementNode[], lastToken: Token) {
    super("while", firstToken, lastToken);
  }
}

export class ForNode extends AstNode {
  constructor(
    firstToken: Token,
    public readonly identifier: IdentifierToken,
    public readonly startExpression: ExpressionNode,
    public readonly endExpression: ExpressionNode,
    public readonly step: ExpressionNode | null,
    public readonly block: StatementNode[],
    lastToken: Token
  ) {
    super("for", firstToken, lastToken);
  }
}

export class ForEachNode extends AstNode {
  constructor(
    firstToken: Token,
    public readonly identifier: IdentifierToken,
    public readonly array: ExpressionNode,
    public readonly block: StatementNode[],
    lastToken: Token
  ) {
    super("for each", firstToken, lastToken);
  }
}

export class DoNode extends AstNode {
  constructor(firstToken: Token, public readonly condition: ExpressionNode, public readonly block: StatementNode[]) {
    super("do", firstToken, condition.lastToken);
  }
}

export type ExpressionNode =
  | TernaryOperatorNode
  | BinaryOperatorNode
  | UnaryOperatorNode
  | IsOperatorNode
  | StringLiteralNode
  | NumberLiteralNode
  | BooleanLiteralNode
  | NothingLiteralNode
  | ArrayLiteralNode
  | MapLiteralNode
  | TupleLiteralNode
  | FunctionLiteralNode
  | VariableAccessNode
  | MemberAccessNode
  | MapOrArrayAccessNode
  | FunctionCallNode
  | MethodCallNode;

export class TernaryOperatorNode extends AstNode {
  constructor(
    public readonly condition: ExpressionNode,
    public readonly trueExpression: ExpressionNode,
    public readonly falseExpression: ExpressionNode
  ) {
    super("ternary operator", condition.firstToken, falseExpression.lastToken);
  }
}

export class BinaryOperatorNode extends AstNode {
  constructor(
    public readonly leftExpression: ExpressionNode,
    public readonly operator: OperatorToken,
    public readonly rightExpression: ExpressionNode
  ) {
    super("binary operator", leftExpression.firstToken, rightExpression.lastToken);
  }
}

export class UnaryOperatorNode extends AstNode {
  constructor(public readonly operator: OperatorToken, public readonly expression: ExpressionNode) {
    super("unary operator", operator, expression.lastToken);
  }
}

export class IsOperatorNode extends AstNode {
  constructor(public readonly leftExpression: ExpressionNode, public readonly type: TypeSpecifierNode[]) {
    super("is operator", leftExpression.firstToken, type[type.length - 1].lastToken);
  }
}

export class StringLiteralNode extends AstNode {
  constructor(public readonly token: StringToken) {
    super("string", token, token);
  }
}

export class NumberLiteralNode extends AstNode {
  constructor(public readonly token: NumberToken) {
    super("number", token, token);
  }
}

export class BooleanLiteralNode extends AstNode {
  constructor(public readonly token: BoolToken) {
    super("boolean", token, token);
  }
}

export class NothingLiteralNode extends AstNode {
  constructor(public readonly token: NothingToken) {
    super("nothing", token, token);
  }
}

export class ArrayLiteralNode extends AstNode {
  constructor(firstToken: Token, public readonly elements: ExpressionNode[], lastToken: Token) {
    super("array literal", firstToken, lastToken);
  }
}

export class MapLiteralNode extends AstNode {
  constructor(firstToken: Token, public readonly keys: StringToken[], values: ExpressionNode[], lastToken: Token) {
    super("map literal", firstToken, lastToken);
  }
}

export class TupleLiteralNode extends AstNode {
  constructor(firstToken: Token, public readonly fieldNames: IdentifierToken[], fieldValues: ExpressionNode[], lastToken: Token) {
    super("tuple literal", firstToken, lastToken);
  }
}

export class FunctionLiteralNode extends AstNode {
  constructor(
    firstToken: Token,
    public readonly parameters: NameAndTypeNode[],
    public returnType: TypeSpecifierNode[] | null,
    public readonly code: StatementNode[],
    lastToken: Token
  ) {
    super("function literal", firstToken, lastToken);
  }
}

export class VariableAccessNode extends AstNode {
  constructor(public readonly name: IdentifierToken) {
    super("variable access", name, name);
  }
}

export class MemberAccessNode extends AstNode {
  constructor(public readonly object: ExpressionNode, public readonly member: IdentifierToken) {
    super("member access", object.firstToken, member);
  }
}

export class MapOrArrayAccessNode extends AstNode {
  constructor(public readonly target: ExpressionNode, public readonly keyOrIndex: ExpressionNode, lastToken: Token) {
    super("map or array access", target.firstToken, lastToken);
  }
}

export class FunctionCallNode extends AstNode {
  constructor(public readonly target: ExpressionNode, public readonly args: ExpressionNode[], lastToken: Token) {
    super("function call", target.firstToken, lastToken);
  }
}

export class MethodCallNode extends AstNode {
  constructor(public readonly target: ExpressionNode, public readonly args: ExpressionNode[], lastToken: Token) {
    super("method call", target.firstToken, lastToken);
  }
}

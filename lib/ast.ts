import { BoolToken, IdentifierToken, NothingToken, NumberToken, OperatorToken, StringToken, Token } from "./tokenizer";

export type AstNodeType = TypeSpecifierNode | StatementNode | ExpressionNode | NameAndTypeNode;

export abstract class AstNode {
  constructor(public readonly firstToken: Token, public readonly lastToken: Token) {}

  get start(): number {
    return this.firstToken.start;
  }

  get end(): number {
    return this.lastToken.end;
  }
}

export type TypeSpecifierNode = PlainTypeNode | ArrayTypeNode | MapTypeNode | FunctionTypeNode | TupleTypeNode;

export class PlainTypeNode extends AstNode {
  public readonly kind: "plain type" = "plain type";
  constructor(public readonly typeName: IdentifierToken) {
    super(typeName, typeName);
  }
}

export class ArrayTypeNode extends AstNode {
  public readonly kind: "array type" = "array type";

  constructor(openingBracket: OperatorToken, public readonly elementTypes: TypeSpecifierNode[], closingBracket: OperatorToken) {
    super(openingBracket, closingBracket);
  }
}

export class MapTypeNode extends AstNode {
  public readonly kind: "map type" = "map type";
  constructor(openingCurly: OperatorToken, public readonly valueTypes: TypeSpecifierNode[], closingCurly: OperatorToken) {
    super(openingCurly, closingCurly);
  }
}

export class FunctionTypeNode extends AstNode {
  public readonly kind: "function type" = "function type";
  constructor(firstToken: Token, public readonly parameters: NameAndTypeNode[], public returnType: TypeSpecifierNode[] | null, lastToken: Token) {
    super(firstToken, lastToken);
  }
}

export class TupleTypeNode extends AstNode {
  public readonly kind: "tuple type" = "tuple type";
  constructor(firstToken: Token, public readonly fields: NameAndTypeNode[], lastToken: Token) {
    super(firstToken, lastToken);
  }
}

export class NameAndTypeNode extends AstNode {
  public readonly kind: "name and type" = "name and type";
  constructor(public readonly name: IdentifierToken, public readonly type: TypeSpecifierNode[]) {
    super(name, type[type.length - 1].lastToken);
  }
}

export type StatementNode =
  | VariableNode
  | RecordNode
  | IfNode
  | WhileNode
  | ForEachNode
  | ForNode
  | DoNode
  | ContinueNode
  | BreakNode
  | ReturnNode
  | ExpressionNode;

export class RecordNode extends AstNode {
  public readonly kind: "record declaration" = "record declaration";
  constructor(firstToken: Token, public readonly name: IdentifierToken, public readonly fields: NameAndTypeNode[], lastToken: Token) {
    super(firstToken, lastToken);
  }
}

export class FunctionNode extends AstNode {
  public readonly kind: "function declaration" = "function declaration";
  constructor(
    firstToken: Token,
    public readonly name: IdentifierToken | null,
    public readonly parameters: NameAndTypeNode[],
    public returnType: TypeSpecifierNode[] | null,
    public readonly code: StatementNode[],
    lastToken: Token
  ) {
    super(firstToken, lastToken);
  }
}

export class VariableNode extends AstNode {
  public kind: "variable declaration" = "variable declaration";
  constructor(
    firstToken: Token,
    public readonly identifier: IdentifierToken,
    public type: TypeSpecifierNode[] | null,
    public readonly initializer: ExpressionNode
  ) {
    super(firstToken, initializer.lastToken);
  }
}

export class IfNode extends AstNode {
  public readonly kind: "if" = "if";
  constructor(
    firstToken: Token,
    public readonly condition: ExpressionNode,
    public readonly trueBlock: StatementNode[],
    public readonly elseIfs: IfNode[],
    public readonly falseBlock: StatementNode[],
    lastToken: Token
  ) {
    super(firstToken, lastToken);
  }
}

export class WhileNode extends AstNode {
  public readonly kind: "while" = "while";
  constructor(firstToken: Token, public readonly condition: ExpressionNode, public readonly block: StatementNode[], lastToken: Token) {
    super(firstToken, lastToken);
  }
}

export class ForNode extends AstNode {
  public readonly kind: "for" = "for";
  constructor(
    firstToken: Token,
    public readonly identifier: IdentifierToken,
    public readonly startExpression: ExpressionNode,
    public readonly endExpression: ExpressionNode,
    public readonly step: ExpressionNode | null,
    public readonly block: StatementNode[],
    lastToken: Token
  ) {
    super(firstToken, lastToken);
  }
}

export class ForEachNode extends AstNode {
  public readonly kind: "for each" = "for each";
  constructor(
    firstToken: Token,
    public readonly identifier: IdentifierToken,
    public readonly array: ExpressionNode,
    public readonly block: StatementNode[],
    lastToken: Token
  ) {
    super(firstToken, lastToken);
  }
}

export class DoNode extends AstNode {
  public readonly kind: "do" = "do";
  constructor(firstToken: Token, public readonly condition: ExpressionNode, public readonly block: StatementNode[]) {
    super(firstToken, condition.lastToken);
  }
}

export class ContinueNode extends AstNode {
  public readonly kind: "continue" = "continue";
  constructor(token: Token) {
    super(token, token);
  }
}

export class BreakNode extends AstNode {
  public readonly kind: "break" = "break";
  constructor(token: Token) {
    super(token, token);
  }
}

export class ReturnNode extends AstNode {
  public readonly kind: "return" = "return";
  constructor(firstToken: Token, expression: ExpressionNode | null) {
    super(firstToken, expression ? expression.lastToken : firstToken);
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
  public readonly kind: "ternary operator" = "ternary operator";
  constructor(
    public readonly condition: ExpressionNode,
    public readonly trueExpression: ExpressionNode,
    public readonly falseExpression: ExpressionNode
  ) {
    super(condition.firstToken, falseExpression.lastToken);
  }
}

export class BinaryOperatorNode extends AstNode {
  public readonly kind: "binary operator" = "binary operator";
  constructor(
    public readonly leftExpression: ExpressionNode,
    public readonly operator: OperatorToken,
    public readonly rightExpression: ExpressionNode
  ) {
    super(leftExpression.firstToken, rightExpression.lastToken);
  }
}

export class UnaryOperatorNode extends AstNode {
  public readonly kind: "unary operator" = "unary operator";
  constructor(public readonly operator: OperatorToken, public readonly expression: ExpressionNode) {
    super(operator, expression.lastToken);
  }
}

export class IsOperatorNode extends AstNode {
  public readonly kind: "is operator" = "is operator";
  constructor(public readonly leftExpression: ExpressionNode, public readonly type: TypeSpecifierNode[]) {
    super(leftExpression.firstToken, type[type.length - 1].lastToken);
  }
}

export class StringLiteralNode extends AstNode {
  public readonly kind: "string" = "string";
  constructor(public readonly token: StringToken) {
    super(token, token);
  }
}

export class NumberLiteralNode extends AstNode {
  public readonly kind: "number" = "number";
  constructor(public readonly token: NumberToken) {
    super(token, token);
  }
}

export class BooleanLiteralNode extends AstNode {
  public readonly kind: "boolean" = "boolean";
  constructor(public readonly token: BoolToken) {
    super(token, token);
  }
}

export class NothingLiteralNode extends AstNode {
  public readonly kind: "nothing" = "nothing";
  constructor(public readonly token: NothingToken) {
    super(token, token);
  }
}

export class ArrayLiteralNode extends AstNode {
  public readonly kind: "array literal" = "array literal";
  constructor(firstToken: Token, public readonly elements: ExpressionNode[], lastToken: Token) {
    super(firstToken, lastToken);
  }
}

export class MapLiteralNode extends AstNode {
  public readonly kind: "map literal" = "map literal";
  constructor(firstToken: Token, public readonly keys: StringToken[], values: ExpressionNode[], lastToken: Token) {
    super(firstToken, lastToken);
  }
}

export class TupleLiteralNode extends AstNode {
  public readonly kind: "tuple literal" = "tuple literal";
  constructor(firstToken: Token, public readonly fieldNames: IdentifierToken[], fieldValues: ExpressionNode[], lastToken: Token) {
    super(firstToken, lastToken);
  }
}

export class FunctionLiteralNode extends AstNode {
  public readonly kind: "function literal" = "function literal";
  constructor(
    firstToken: Token,
    public readonly parameters: NameAndTypeNode[],
    public returnType: TypeSpecifierNode[] | null,
    public readonly code: StatementNode[],
    lastToken: Token
  ) {
    super(firstToken, lastToken);
  }
}

export class VariableAccessNode extends AstNode {
  public readonly kind: "variable access" = "variable access";
  constructor(public readonly name: IdentifierToken) {
    super(name, name);
  }
}

export class MemberAccessNode extends AstNode {
  public readonly kind: "member access" = "member access";
  constructor(public readonly object: ExpressionNode, public readonly member: IdentifierToken) {
    super(object.firstToken, member);
  }
}

export class MapOrArrayAccessNode extends AstNode {
  public readonly kind: "map or array access" = "map or array access";
  constructor(public readonly target: ExpressionNode, public readonly keyOrIndex: ExpressionNode, lastToken: Token) {
    super(target.firstToken, lastToken);
  }
}

export class FunctionCallNode extends AstNode {
  public readonly kind: "function call" = "function call";
  constructor(public readonly target: ExpressionNode, public readonly args: ExpressionNode[], lastToken: Token) {
    super(target.firstToken, lastToken);
  }
}

export class MethodCallNode extends AstNode {
  public readonly kind: "method call" = "method call";
  constructor(public readonly target: ExpressionNode, public readonly args: ExpressionNode[], lastToken: Token) {
    super(target.firstToken, lastToken);
  }
}

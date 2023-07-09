import { BoolToken, IdentifierToken, NothingToken, NumberToken, OperatorToken, StringToken, Token } from "./tokenizer";
import { Type, UnknownType } from "./types";

export type AstNode = NameAndTypeNode | TypeSpecifierNode | TopLevelNode;

export type TypeSpecifierNode = NamedTypeNode | ArrayTypeNode | MapTypeNode | FunctionTypeNode | TupleTypeNode | UnionTypeNode | MixinTypeNode;

export type TopLevelNode = FunctionNode | TypeNode | StatementNode;

export type StatementNode =
  | VariableNode
  | IfNode
  | WhileNode
  | ForEachNode
  | ForNode
  | DoNode
  | ContinueNode
  | BreakNode
  | ReturnNode
  | ExpressionNode;

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

export abstract class BaseAstNode {
  public type: Type = UnknownType;

  constructor(public readonly firstToken: Token, public readonly lastToken: Token) {}

  get start(): number {
    return this.firstToken.start;
  }

  get end(): number {
    return this.lastToken.end;
  }
}

export class NamedTypeNode extends BaseAstNode {
  public readonly kind: "named type" = "named type";
  constructor(public readonly name: IdentifierToken) {
    super(name, name);
  }
}

export class ArrayTypeNode extends BaseAstNode {
  public readonly kind: "array type" = "array type";

  constructor(openingBracket: OperatorToken, public readonly elementType: TypeSpecifierNode, closingBracket: OperatorToken) {
    super(openingBracket, closingBracket);
  }
}

export class MapTypeNode extends BaseAstNode {
  public readonly kind: "map type" = "map type";
  constructor(openingCurly: OperatorToken, public readonly valueType: TypeSpecifierNode, closingCurly: OperatorToken) {
    super(openingCurly, closingCurly);
  }
}

export class FunctionTypeNode extends BaseAstNode {
  public readonly kind: "function type" = "function type";
  constructor(firstToken: Token, public readonly parameters: NameAndTypeNode[], public returnType: TypeSpecifierNode | null, lastToken: Token) {
    super(firstToken, lastToken);
  }
}

export class TupleTypeNode extends BaseAstNode {
  public readonly kind: "tuple type" = "tuple type";
  constructor(firstToken: Token, public readonly fields: NameAndTypeNode[], lastToken: Token) {
    super(firstToken, lastToken);
  }
}

export class UnionTypeNode extends BaseAstNode {
  public readonly kind: "union type" = "union type";
  constructor(public readonly unionTypes: TypeSpecifierNode[]) {
    super(unionTypes[0].firstToken, unionTypes[unionTypes.length - 1].lastToken);
  }
}

export class MixinTypeNode extends BaseAstNode {
  public readonly kind: "mixin type" = "mixin type";
  constructor(public readonly mixinTypes: TypeSpecifierNode[]) {
    super(mixinTypes[0].firstToken, mixinTypes[mixinTypes.length - 1].lastToken);
  }
}

export class TypeNode extends BaseAstNode {
  public readonly kind: "type declaration" = "type declaration";

  constructor(firstToken: Token, public readonly name: IdentifierToken, public readonly typeNode: TypeSpecifierNode) {
    super(firstToken, typeNode.lastToken);
  }
}

export class NameAndTypeNode extends BaseAstNode {
  public readonly kind: "name and type" = "name and type";
  constructor(public readonly name: IdentifierToken, public readonly typeNode: TypeSpecifierNode) {
    super(name, typeNode.lastToken);
  }
}

export class FunctionNode extends BaseAstNode {
  public readonly kind: "function declaration" = "function declaration";
  constructor(
    firstToken: Token,
    public readonly name: IdentifierToken,
    public readonly parameters: NameAndTypeNode[],
    public returnType: TypeSpecifierNode | null,
    public readonly code: StatementNode[],
    lastToken: Token
  ) {
    super(firstToken, lastToken);
  }
}

export class VariableNode extends BaseAstNode {
  public kind: "variable declaration" = "variable declaration";
  constructor(
    firstToken: Token,
    public readonly identifier: IdentifierToken,
    public typeNode: TypeSpecifierNode | null,
    public readonly initializer: ExpressionNode
  ) {
    super(firstToken, initializer.lastToken);
  }
}

export class IfNode extends BaseAstNode {
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

export class WhileNode extends BaseAstNode {
  public readonly kind: "while" = "while";
  constructor(firstToken: Token, public readonly condition: ExpressionNode, public readonly block: StatementNode[], lastToken: Token) {
    super(firstToken, lastToken);
  }
}

export class ForNode extends BaseAstNode {
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

export class ForEachNode extends BaseAstNode {
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

export class DoNode extends BaseAstNode {
  public readonly kind: "do" = "do";
  constructor(firstToken: Token, public readonly condition: ExpressionNode, public readonly block: StatementNode[]) {
    super(firstToken, condition.lastToken);
  }
}

export class ContinueNode extends BaseAstNode {
  public readonly kind: "continue" = "continue";
  constructor(token: Token) {
    super(token, token);
  }
}

export class BreakNode extends BaseAstNode {
  public readonly kind: "break" = "break";
  constructor(token: Token) {
    super(token, token);
  }
}

export class ReturnNode extends BaseAstNode {
  public readonly kind: "return" = "return";
  constructor(firstToken: Token, expression: ExpressionNode | null) {
    super(firstToken, expression ? expression.lastToken : firstToken);
  }
}

export class TernaryOperatorNode extends BaseAstNode {
  public readonly kind: "ternary operator" = "ternary operator";
  constructor(
    public readonly condition: ExpressionNode,
    public readonly trueExpression: ExpressionNode,
    public readonly falseExpression: ExpressionNode
  ) {
    super(condition.firstToken, falseExpression.lastToken);
  }
}

export class BinaryOperatorNode extends BaseAstNode {
  public readonly kind: "binary operator" = "binary operator";
  constructor(
    public readonly leftExpression: ExpressionNode,
    public readonly operator: OperatorToken,
    public readonly rightExpression: ExpressionNode
  ) {
    super(leftExpression.firstToken, rightExpression.lastToken);
  }
}

export class UnaryOperatorNode extends BaseAstNode {
  public readonly kind: "unary operator" = "unary operator";
  constructor(public readonly operator: OperatorToken, public readonly expression: ExpressionNode) {
    super(operator, expression.lastToken);
  }
}

export class IsOperatorNode extends BaseAstNode {
  public readonly kind: "is operator" = "is operator";
  constructor(public readonly leftExpression: ExpressionNode, public readonly typeNode: TypeSpecifierNode) {
    super(leftExpression.firstToken, typeNode.lastToken);
  }
}

export class StringLiteralNode extends BaseAstNode {
  public readonly kind: "string" = "string";
  constructor(public readonly token: StringToken) {
    super(token, token);
  }
}

export class NumberLiteralNode extends BaseAstNode {
  public readonly kind: "number" = "number";
  constructor(public readonly token: NumberToken) {
    super(token, token);
  }
}

export class BooleanLiteralNode extends BaseAstNode {
  public readonly kind: "boolean" = "boolean";
  constructor(public readonly token: BoolToken) {
    super(token, token);
  }
}

export class NothingLiteralNode extends BaseAstNode {
  public readonly kind: "nothing" = "nothing";
  constructor(public readonly token: NothingToken) {
    super(token, token);
  }
}

export class ArrayLiteralNode extends BaseAstNode {
  public readonly kind: "array literal" = "array literal";
  constructor(firstToken: Token, public readonly elements: ExpressionNode[], lastToken: Token) {
    super(firstToken, lastToken);
  }
}

export class MapLiteralNode extends BaseAstNode {
  public readonly kind: "map literal" = "map literal";
  constructor(firstToken: Token, public readonly keys: StringToken[], public readonly values: ExpressionNode[], lastToken: Token) {
    super(firstToken, lastToken);
  }
}

export class TupleLiteralNode extends BaseAstNode {
  public readonly kind: "tuple literal" = "tuple literal";
  constructor(firstToken: Token, public readonly fieldNames: IdentifierToken[], public readonly fieldValues: ExpressionNode[], lastToken: Token) {
    super(firstToken, lastToken);
  }
}

export class FunctionLiteralNode extends BaseAstNode {
  public readonly kind: "function literal" = "function literal";
  constructor(
    firstToken: Token,
    public readonly parameters: NameAndTypeNode[],
    public returnType: TypeSpecifierNode | null,
    public readonly code: StatementNode[],
    lastToken: Token
  ) {
    super(firstToken, lastToken);
  }
}

export class VariableAccessNode extends BaseAstNode {
  public readonly kind: "variable access" = "variable access";
  constructor(public readonly name: IdentifierToken) {
    super(name, name);
  }
}

export class MemberAccessNode extends BaseAstNode {
  public readonly kind: "member access" = "member access";
  constructor(public readonly object: ExpressionNode, public readonly member: IdentifierToken) {
    super(object.firstToken, member);
  }
}

export class MapOrArrayAccessNode extends BaseAstNode {
  public readonly kind: "map or array access" = "map or array access";
  constructor(public readonly target: ExpressionNode, public readonly keyOrIndex: ExpressionNode, lastToken: Token) {
    super(target.firstToken, lastToken);
  }
}

export class FunctionCallNode extends BaseAstNode {
  public readonly kind: "function call" = "function call";
  constructor(public readonly target: ExpressionNode, public readonly args: ExpressionNode[], lastToken: Token) {
    super(target.firstToken, lastToken);
  }
}

export class MethodCallNode extends BaseAstNode {
  public readonly kind: "method call" = "method call";
  constructor(public readonly target: ExpressionNode, public readonly args: ExpressionNode[], lastToken: Token) {
    super(target.firstToken, lastToken);
  }
}

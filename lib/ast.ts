import { BoolToken, CommentToken, IdentifierToken, NothingToken, NumberToken, OperatorToken, StringToken } from "./tokenizer";

export abstract class AstNode {
  constructor(public readonly nodeLabel: string) {}
}

export type TypeSpecifierNode = PlainTypeNode | ArrayTypeNode | MapTypeNode | FunctionTypeNode;

export class PlainTypeNode extends AstNode {
  constructor(public readonly typeName: IdentifierToken) {
    super("plain type");
  }
}

export class ArrayTypeNode extends AstNode {
  constructor(public readonly elementTypes: TypeSpecifierNode[]) {
    super("array type");
  }
}

export class MapTypeNode extends AstNode {
  constructor(public readonly valueTypes: TypeSpecifierNode[]) {
    super("map type");
  }
}

export class FunctionTypeNode extends AstNode {
  constructor(public readonly parameters: NameAndTypeNode[], public returnType: TypeSpecifierNode[] | null) {
    super("function type");
  }
}

export class NameAndTypeNode extends AstNode {
  constructor(public readonly name: IdentifierToken, public readonly type: TypeSpecifierNode[]) {
    super("name and type");
  }
}

export class TypeDeclarationNode extends AstNode {
  constructor(public readonly name: IdentifierToken, public readonly fields: (NameAndTypeNode | CommentNode)[]) {
    super("type declaration");
  }
}

export class FunctionNode extends AstNode {
  constructor(
    public readonly name: IdentifierToken | null,
    public readonly parameters: NameAndTypeNode[],
    public returnType: TypeSpecifierNode[] | null,
    public readonly code: StatementNode[]
  ) {
    super("function declaration");
  }
}

export type StatementNode = CommentNode | VariableNode | IfNode | WhileNode | ForEachNode | ForNode | DoNode | ExpressionNode;

export class CommentNode extends AstNode {
  constructor(public readonly lines: CommentToken[]) {
    super("comment");
  }
}

export class VariableNode extends AstNode {
  constructor(public readonly identifier: IdentifierToken, public readonly initializer: ExpressionNode, public type: TypeSpecifierNode[] | null) {
    super("variable declaration");
  }
}

export class IfNode extends AstNode {
  constructor(
    public readonly condition: ExpressionNode,
    public readonly trueBlock: StatementNode[],
    public readonly elseIfs: IfNode[],
    public readonly falseBlock: StatementNode[]
  ) {
    super("if");
  }
}

export class WhileNode extends AstNode {
  constructor(public readonly condition: ExpressionNode, public readonly block: StatementNode[]) {
    super("while");
  }
}

export class ForNode extends AstNode {
  constructor(
    public readonly identifier: IdentifierToken,
    public readonly start: ExpressionNode,
    public readonly end: ExpressionNode,
    public readonly step: ExpressionNode | null,
    public readonly block: StatementNode[]
  ) {
    super("for");
  }
}

export class ForEachNode extends AstNode {
  constructor(public readonly identifier: IdentifierToken, public readonly array: ExpressionNode, public readonly block: StatementNode[]) {
    super("for each");
  }
}

export class DoNode extends AstNode {
  constructor(public readonly condition: AstNode, public readonly block: StatementNode[]) {
    super("do");
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
    super("ternary operator");
  }
}

export class BinaryOperatorNode extends AstNode {
  constructor(
    public readonly leftExpression: ExpressionNode,
    public readonly operator: OperatorToken,
    public readonly rightExpression: ExpressionNode
  ) {
    super("binary operator");
  }
}

export class UnaryOperatorNode extends AstNode {
  constructor(public readonly operator: OperatorToken, public readonly expression: ExpressionNode) {
    super("unary operator");
  }
}

export class IsOperatorNode extends AstNode {
  constructor(public readonly leftExpression: ExpressionNode, public readonly type: TypeSpecifierNode[]) {
    super("is operator");
  }
}

export class StringLiteralNode extends AstNode {
  constructor(public readonly token: StringToken) {
    super("string");
  }
}

export class NumberLiteralNode extends AstNode {
  constructor(public readonly token: NumberToken) {
    super("number");
  }
}

export class BooleanLiteralNode extends AstNode {
  constructor(public readonly token: BoolToken) {
    super("boolean");
  }
}

export class NothingLiteralNode extends AstNode {
  constructor(public readonly token: NothingToken) {
    super("nothing");
  }
}

export class ArrayLiteralNode extends AstNode {
  constructor(public readonly elements: ExpressionNode[]) {
    super("array literal");
  }
}

export class MapLiteralNode extends AstNode {
  constructor(public readonly keyValues: ExpressionNode[]) {
    super("map literal");
  }
}

export class FunctionLiteralNode extends AstNode {
  constructor(public readonly parameters: NameAndTypeNode[], public returnType: TypeSpecifierNode[] | null, public readonly code: StatementNode[]) {
    super("function literal");
  }
}

export class VariableAccessNode extends AstNode {
  constructor(public readonly name: IdentifierToken) {
    super("variable access");
  }
}

export class MemberAccessNode extends AstNode {
  constructor(public readonly object: ExpressionNode, public readonly member: IdentifierToken) {
    super("member access");
  }
}

export class MapOrArrayAccessNode extends AstNode {
  constructor(public readonly target: ExpressionNode, public readonly keyOrIndex: ExpressionNode) {
    super("map or array access");
  }
}

export class FunctionCallNode extends AstNode {
  constructor(public readonly target: ExpressionNode, public readonly args: ExpressionNode[]) {
    super("function call");
  }
}

export class MethodCallNode extends AstNode {
  constructor(public readonly target: ExpressionNode, public readonly args: ExpressionNode[]) {
    super("method call");
  }
}

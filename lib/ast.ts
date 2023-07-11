import { SourceLocation } from "./source";
import { BoolToken, IdentifierToken, NothingToken, NumberToken, OperatorToken, StringToken, Token } from "./tokenizer";
import { Type, UnknownType } from "./types";

export type AstNode = NameAndTypeNode | TypeSpecifierNode | TopLevelNode;

export type TypeSpecifierNode = TypeReferenceNode | ListTypeNode | MapTypeNode | FunctionTypeNode | RecordTypeNode | UnionTypeNode | MixinTypeNode;

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
  | ListLiteralNode
  | MapLiteralNode
  | RecordLiteralNode
  | FunctionLiteralNode
  | VariableAccessNode
  | MemberAccessNode
  | MapOrListAccessNode
  | FunctionCallNode
  | MethodCallNode;

export abstract class BaseAstNode {
  public type: Type = UnknownType;

  constructor(public readonly location: SourceLocation) {}
}

export class TypeReferenceNode extends BaseAstNode {
  public readonly kind: "type reference" = "type reference";
  constructor(public readonly name: IdentifierToken) {
    super(name.location);
  }
}

export class ListTypeNode extends BaseAstNode {
  public readonly kind: "list type" = "list type";

  constructor(openingBracket: OperatorToken, public readonly elementType: TypeSpecifierNode, closingBracket: OperatorToken) {
    super(SourceLocation.from(openingBracket.location, closingBracket.location));
  }
}

export class MapTypeNode extends BaseAstNode {
  public readonly kind: "map type" = "map type";
  constructor(openingCurly: OperatorToken, public readonly valueType: TypeSpecifierNode, closingCurly: OperatorToken) {
    super(SourceLocation.from(openingCurly.location, closingCurly.location));
  }
}

export class FunctionTypeNode extends BaseAstNode {
  public readonly kind: "function type" = "function type";
  constructor(
    firstToken: Token,
    public readonly parameters: NameAndTypeNode[],
    public returnType: TypeSpecifierNode | null,
    lastLocation: SourceLocation
  ) {
    super(SourceLocation.from(firstToken.location, lastLocation));
  }
}

export class RecordTypeNode extends BaseAstNode {
  public readonly kind: "record type" = "record type";
  constructor(firstToken: Token, public readonly fields: NameAndTypeNode[], lastToken: Token) {
    super(SourceLocation.from(firstToken.location, lastToken.location));
  }
}

export class UnionTypeNode extends BaseAstNode {
  public readonly kind: "union type" = "union type";
  constructor(public readonly unionTypes: TypeSpecifierNode[]) {
    super(SourceLocation.from(unionTypes[0].location, unionTypes[unionTypes.length - 1].location));
  }
}

export class MixinTypeNode extends BaseAstNode {
  public readonly kind: "mixin type" = "mixin type";
  constructor(public readonly mixinTypes: TypeSpecifierNode[]) {
    super(SourceLocation.from(mixinTypes[0].location, mixinTypes[mixinTypes.length - 1].location));
  }
}

export class TypeNode extends BaseAstNode {
  public readonly kind: "type declaration" = "type declaration";

  constructor(firstToken: Token, public readonly name: IdentifierToken, public readonly typeNode: TypeSpecifierNode) {
    super(SourceLocation.from(firstToken.location, typeNode.location));
  }
}

export class NameAndTypeNode extends BaseAstNode {
  public readonly kind: "name and type" = "name and type";
  constructor(public readonly name: IdentifierToken, public readonly typeNode: TypeSpecifierNode) {
    super(SourceLocation.from(name.location, typeNode.location));
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
    super(SourceLocation.from(firstToken.location, lastToken.location));
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
    super(SourceLocation.from(firstToken.location, initializer.location));
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
    lastLocation: SourceLocation
  ) {
    super(SourceLocation.from(firstToken.location, lastLocation));
  }
}

export class WhileNode extends BaseAstNode {
  public readonly kind: "while" = "while";
  constructor(firstToken: Token, public readonly condition: ExpressionNode, public readonly block: StatementNode[], lastToken: Token) {
    super(SourceLocation.from(firstToken.location, lastToken.location));
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
    super(SourceLocation.from(firstToken.location, lastToken.location));
  }
}

export class ForEachNode extends BaseAstNode {
  public readonly kind: "for each" = "for each";
  constructor(
    firstToken: Token,
    public readonly identifier: IdentifierToken,
    public readonly list: ExpressionNode,
    public readonly block: StatementNode[],
    lastToken: Token
  ) {
    super(SourceLocation.from(firstToken.location, lastToken.location));
  }
}

export class DoNode extends BaseAstNode {
  public readonly kind: "do" = "do";
  constructor(firstToken: Token, public readonly condition: ExpressionNode, public readonly block: StatementNode[]) {
    super(SourceLocation.from(firstToken.location, condition.location));
  }
}

export class ContinueNode extends BaseAstNode {
  public readonly kind: "continue" = "continue";
  constructor(token: Token) {
    super(token.location);
  }
}

export class BreakNode extends BaseAstNode {
  public readonly kind: "break" = "break";
  constructor(token: Token) {
    super(token.location);
  }
}

export class ReturnNode extends BaseAstNode {
  public readonly kind: "return" = "return";
  constructor(firstToken: Token, public readonly expression: ExpressionNode | null) {
    super(SourceLocation.from(firstToken.location, expression ? expression.location : firstToken.location));
  }
}

export class TernaryOperatorNode extends BaseAstNode {
  public readonly kind: "ternary operator" = "ternary operator";
  constructor(
    public readonly condition: ExpressionNode,
    public readonly trueExpression: ExpressionNode,
    public readonly falseExpression: ExpressionNode
  ) {
    super(SourceLocation.from(condition.location, falseExpression.location));
  }
}

export class BinaryOperatorNode extends BaseAstNode {
  public readonly kind: "binary operator" = "binary operator";
  constructor(
    public readonly leftExpression: ExpressionNode,
    public readonly operator: OperatorToken,
    public readonly rightExpression: ExpressionNode
  ) {
    super(SourceLocation.from(leftExpression.location, rightExpression.location));
  }
}

export class UnaryOperatorNode extends BaseAstNode {
  public readonly kind: "unary operator" = "unary operator";
  constructor(public readonly operator: OperatorToken, public readonly expression: ExpressionNode) {
    super(SourceLocation.from(operator.location, expression.location));
  }
}

export class IsOperatorNode extends BaseAstNode {
  public readonly kind: "is operator" = "is operator";
  constructor(public readonly leftExpression: ExpressionNode, public readonly typeNode: TypeSpecifierNode) {
    super(SourceLocation.from(leftExpression.location, typeNode.location));
  }
}

export class StringLiteralNode extends BaseAstNode {
  public readonly kind: "string" = "string";
  constructor(public readonly token: StringToken) {
    super(token.location);
  }
}

export class NumberLiteralNode extends BaseAstNode {
  public readonly kind: "number" = "number";
  constructor(public readonly token: NumberToken) {
    super(token.location);
  }
}

export class BooleanLiteralNode extends BaseAstNode {
  public readonly kind: "boolean" = "boolean";
  constructor(public readonly token: BoolToken) {
    super(token.location);
  }
}

export class NothingLiteralNode extends BaseAstNode {
  public readonly kind: "nothing" = "nothing";
  constructor(public readonly token: NothingToken) {
    super(token.location);
  }
}

export class ListLiteralNode extends BaseAstNode {
  public readonly kind: "list literal" = "list literal";
  constructor(firstToken: Token, public readonly elements: ExpressionNode[], lastToken: Token) {
    super(SourceLocation.from(firstToken.location, lastToken.location));
  }
}

export class MapLiteralNode extends BaseAstNode {
  public readonly kind: "map literal" = "map literal";
  constructor(firstToken: Token, public readonly keys: StringToken[], public readonly values: ExpressionNode[], lastToken: Token) {
    super(SourceLocation.from(firstToken.location, lastToken.location));
  }
}

export class RecordLiteralNode extends BaseAstNode {
  public readonly kind: "record literal" = "record literal";
  constructor(firstToken: Token, public readonly fieldNames: IdentifierToken[], public readonly fieldValues: ExpressionNode[], lastToken: Token) {
    super(SourceLocation.from(firstToken.location, lastToken.location));
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
    super(SourceLocation.from(firstToken.location, lastToken.location));
  }
}

export class VariableAccessNode extends BaseAstNode {
  public readonly kind: "variable access" = "variable access";
  constructor(public readonly name: IdentifierToken) {
    super(name.location);
  }
}

export class MemberAccessNode extends BaseAstNode {
  public readonly kind: "member access" = "member access";
  constructor(public readonly object: ExpressionNode, public readonly member: IdentifierToken) {
    super(SourceLocation.from(object.location, member.location));
  }
}

export class MapOrListAccessNode extends BaseAstNode {
  public readonly kind: "map or list access" = "map or list access";
  constructor(public readonly target: ExpressionNode, public readonly keyOrIndex: ExpressionNode, lastToken: Token) {
    super(SourceLocation.from(target.location, lastToken.location));
  }
}

export class FunctionCallNode extends BaseAstNode {
  public readonly kind: "function call" = "function call";
  constructor(public readonly target: ExpressionNode, public readonly args: ExpressionNode[], lastToken: Token) {
    super(SourceLocation.from(target.location, lastToken.location));
  }
}

export class MethodCallNode extends BaseAstNode {
  public readonly kind: "method call" = "method call";
  constructor(public readonly target: ExpressionNode, public readonly args: ExpressionNode[], lastToken: Token) {
    super(SourceLocation.from(target.location, lastToken.location));
  }
}

function assertNever(x: never) {
  throw new Error("Unexpected object: " + x);
}

export function traverseAst(node: AstNode, callback: (node: AstNode) => boolean) {
  if (!callback(node)) return;
  switch (node.kind) {
    case "nothing":
      break;
    case "string":
      break;
    case "number":
      break;
    case "boolean":
      break;
    case "name and type":
      traverseAst(node.typeNode, callback);
      break;
    case "type reference":
      break;
    case "list type":
      traverseAst(node.elementType, callback);
      break;
    case "map type":
      traverseAst(node.valueType, callback);
      break;
    case "function type":
      for (const param of node.parameters) {
        traverseAst(param, callback);
      }
      if (node.returnType) traverseAst(node.returnType, callback);
      break;
    case "record type":
      for (const field of node.fields) {
        traverseAst(field, callback);
      }
      break;
    case "union type":
      for (const type of node.unionTypes) {
        traverseAst(type, callback);
      }
      break;
    case "mixin type":
      for (const type of node.mixinTypes) {
        traverseAst(type, callback);
      }
      break;
    case "function declaration":
      for (const param of node.parameters) {
        traverseAst(param, callback);
      }
      if (node.returnType) traverseAst(node.returnType, callback);
      for (const statement of node.code) {
        traverseAst(statement, callback);
      }
      break;
    case "type declaration":
      traverseAst(node.typeNode, callback);
      break;
    case "variable declaration":
      if (node.typeNode) traverseAst(node.typeNode, callback);
      traverseAst(node.initializer, callback);
      break;
    case "if":
      traverseAst(node.condition, callback);
      for (const statement of node.trueBlock) {
        traverseAst(statement, callback);
      }
      for (const elseIf of node.elseIfs) {
        traverseAst(elseIf, callback);
      }
      for (const statement of node.falseBlock) {
        traverseAst(statement, callback);
      }
      break;
    case "while":
      traverseAst(node.condition, callback);
      for (const statement of node.block) {
        traverseAst(statement, callback);
      }
      break;
    case "for each":
      traverseAst(node.list, callback);
      for (const statement of node.block) {
        traverseAst(statement, callback);
      }
      break;
    case "for":
      traverseAst(node.startExpression, callback);
      traverseAst(node.endExpression, callback);
      if (node.step) traverseAst(node.step, callback);
      for (const statement of node.block) {
        traverseAst(statement, callback);
      }
      break;
    case "do":
      for (const statement of node.block) {
        traverseAst(statement, callback);
      }
      traverseAst(node.condition, callback);
      break;
    case "continue":
      break;
    case "break":
      break;
    case "return":
      if (node.expression) traverseAst(node.expression, callback);
      break;
    case "ternary operator":
      traverseAst(node.condition, callback);
      traverseAst(node.trueExpression, callback);
      traverseAst(node.falseExpression, callback);
      break;
    case "binary operator":
      traverseAst(node.leftExpression, callback);
      traverseAst(node.rightExpression, callback);
      break;
    case "unary operator":
      traverseAst(node.expression, callback);
      break;
    case "is operator":
      traverseAst(node.leftExpression, callback);
      traverseAst(node.typeNode, callback);
      break;
    case "list literal":
      for (const element of node.elements) {
        traverseAst(element, callback);
      }
      break;
    case "map literal":
      for (let i = 0; i < node.values.length; i++) {
        traverseAst(node.values[i], callback);
      }
      break;
    case "record literal":
      for (let i = 0; i < node.fieldValues.length; i++) {
        traverseAst(node.fieldValues[i], callback);
      }
      break;
    case "function literal":
      for (const param of node.parameters) {
        traverseAst(param, callback);
      }
      if (node.returnType) traverseAst(node.returnType, callback);
      for (const statement of node.code) {
        traverseAst(statement, callback);
      }
      break;
    case "variable access":
      break;
    case "member access":
      traverseAst(node.object, callback);
      break;
    case "map or list access":
      traverseAst(node.target, callback);
      traverseAst(node.keyOrIndex, callback);
      break;
    case "function call":
      for (const arg of node.args) {
        traverseAst(arg, callback);
      }
      break;
    case "method call":
      traverseAst(node.target, callback);
      for (const arg of node.args) {
        traverseAst(arg, callback);
      }
      break;
    default:
      assertNever(node);
  }
}

import { SourceLocation } from "./source";
import { BoolToken, IdentifierToken, NothingToken, NumberToken, OperatorToken, StringToken, Token } from "./tokenizer";
import { Type, UnknownType } from "./types";

export type AstNode = ImportNode | ImportedNameNode | InternalNode | TopLevelNode;

export type TopLevelNode = FunctionNode | TypeNode | StatementNode;

export type InternalNode = NameAndTypeNode | LoopVariable | TypeSpecifierNode;

export type TypeSpecifierNode = TypeReferenceNode | ListTypeNode | MapTypeNode | FunctionTypeNode | RecordTypeNode | UnionTypeNode | MixinTypeNode;

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
  | AsOperatorNode
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
  constructor(public readonly name: IdentifierToken, public readonly genericTypeBindings: TypeSpecifierNode[], closingBracket: OperatorToken | null) {
    super(SourceLocation.from(name.location, closingBracket ? closingBracket.location : name.location));
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

  constructor(
    firstToken: Token,
    public readonly name: IdentifierToken,
    public readonly genericTypeNames: IdentifierToken[],
    public readonly typeNode: TypeSpecifierNode,
    public readonly exported: boolean
  ) {
    super(SourceLocation.from(firstToken.location, typeNode.location));
  }
}

export class NameAndTypeNode extends BaseAstNode {
  public readonly kind: "name and type" = "name and type";
  constructor(public readonly name: IdentifierToken, public readonly typeNode: TypeSpecifierNode) {
    super(SourceLocation.from(name.location, typeNode.location));
  }
}

export class ImportedNameNode extends BaseAstNode {
  public readonly kind: "imported name" = "imported name";
  constructor(public readonly name: IdentifierToken, public readonly alias: IdentifierToken | null) {
    super(SourceLocation.from(name.location, alias ? alias.location : name.location));
  }
}

export class ImportNode extends BaseAstNode {
  public readonly kind: "import" = "import";

  constructor(firstToken: Token, public readonly importedNames: ImportedNameNode[], public readonly path: StringToken) {
    super(SourceLocation.from(firstToken.location, path.location));
  }
}

export class FunctionNode extends BaseAstNode {
  public readonly kind: "function declaration" = "function declaration";
  constructor(
    public readonly location: SourceLocation,
    public readonly name: IdentifierToken,
    public readonly genericTypeNames: IdentifierToken[],
    public readonly parameters: NameAndTypeNode[],
    public returnType: TypeSpecifierNode | null,
    public readonly code: StatementNode[],
    public readonly exported: boolean,
    public readonly external: boolean,
    public isBeingChecked = false
  ) {
    super(location);
  }
}

export class VariableNode extends BaseAstNode {
  public kind: "variable declaration" = "variable declaration";
  constructor(
    firstToken: Token,
    public readonly name: IdentifierToken,
    public typeNode: TypeSpecifierNode | null,
    public readonly initializer: ExpressionNode,
    public readonly exported: boolean,
    public readonly constant: boolean,
    public moduleVariable = false
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

export class LoopVariable extends BaseAstNode {
  public readonly kind: "loop variable" = "loop variable";
  constructor(public readonly name: IdentifierToken) {
    super(name.location);
  }
}

export class ForNode extends BaseAstNode {
  public readonly kind: "for" = "for";
  constructor(
    firstToken: Token,
    public readonly loopVariable: LoopVariable,
    public readonly from: ExpressionNode,
    public readonly to: ExpressionNode,
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
    public readonly loopVariable: LoopVariable,
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

export class AsOperatorNode extends BaseAstNode {
  public readonly kind: "as operator" = "as operator";
  constructor(public readonly leftExpression: ExpressionNode, public readonly typeNode: TypeSpecifierNode) {
    super(SourceLocation.from(leftExpression.location, typeNode.location));
  }
}

export class StringLiteralNode extends BaseAstNode {
  public readonly kind: "string literal" = "string literal";
  constructor(public readonly token: StringToken) {
    super(token.location);
  }
}

export class NumberLiteralNode extends BaseAstNode {
  public readonly kind: "number literal" = "number literal";
  constructor(public readonly token: NumberToken) {
    super(token.location);
  }
}

export class BooleanLiteralNode extends BaseAstNode {
  public readonly kind: "boolean literal" = "boolean literal";
  constructor(public readonly token: BoolToken) {
    super(token.location);
  }
}

export class NothingLiteralNode extends BaseAstNode {
  public readonly kind: "nothing literal" = "nothing literal";
  constructor(public readonly token: NothingToken) {
    super(token.location);
  }
}

export class ListLiteralNode extends BaseAstNode {
  public readonly kind: "list literal" = "list literal";
  constructor(firstToken: Token, public readonly elements: ExpressionNode[], public readonly typeNode: TypeSpecifierNode | null, lastToken: Token) {
    super(SourceLocation.from(firstToken.location, lastToken.location));
  }
}

export class MapLiteralNode extends BaseAstNode {
  public readonly kind: "map literal" = "map literal";
  constructor(
    firstToken: Token,
    public readonly keys: StringToken[],
    public readonly values: ExpressionNode[],
    public readonly typeNode: TypeSpecifierNode | null,
    lastToken: Token
  ) {
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
    lastLocation: SourceLocation
  ) {
    super(SourceLocation.from(firstToken.location, lastLocation));
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
  constructor(openingBracket: Token, public readonly target: ExpressionNode, public readonly keyOrIndex: ExpressionNode, lastToken: Token) {
    super(SourceLocation.from(openingBracket.location, lastToken.location));
  }
}

export class FunctionCallNode extends BaseAstNode {
  public readonly kind: "function call" = "function call";
  constructor(
    public readonly target: VariableAccessNode | MapOrListAccessNode | FunctionLiteralNode,
    public readonly args: ExpressionNode[],
    lastToken: Token
  ) {
    super(SourceLocation.from(target.location, lastToken.location));
  }
}

export class MethodCallNode extends BaseAstNode {
  public readonly kind: "method call" = "method call";
  constructor(public readonly target: MemberAccessNode, public readonly args: ExpressionNode[], lastToken: Token) {
    super(SourceLocation.from(target.location, lastToken.location));
  }
}

function assertNever(x: never) {
  throw new Error("Unexpected object: " + x);
}

export function traverseAst(node: AstNode, parent: AstNode | null, callback: (node: AstNode, parent: AstNode | null) => boolean) {
  if (!callback(node, parent)) return;
  switch (node.kind) {
    case "nothing literal":
      break;
    case "string literal":
      break;
    case "number literal":
      break;
    case "boolean literal":
      break;
    case "name and type":
      traverseAst(node.typeNode, node, callback);
      break;
    case "type reference":
      break;
    case "list type":
      traverseAst(node.elementType, node, callback);
      break;
    case "map type":
      traverseAst(node.valueType, node, callback);
      break;
    case "function type":
      for (const param of node.parameters) {
        traverseAst(param, node, callback);
      }
      if (node.returnType) traverseAst(node.returnType, node, callback);
      break;
    case "record type":
      for (const field of node.fields) {
        traverseAst(field, node, callback);
      }
      break;
    case "union type":
      for (const type of node.unionTypes) {
        traverseAst(type, node, callback);
      }
      break;
    case "mixin type":
      for (const type of node.mixinTypes) {
        traverseAst(type, node, callback);
      }
      break;
    case "import":
      for (const importedName of node.importedNames) {
        traverseAst(importedName, node, callback);
      }
      break;
    case "imported name":
      break;
    case "function declaration":
      for (const param of node.parameters) {
        traverseAst(param, node, callback);
      }
      if (node.returnType) traverseAst(node.returnType, node, callback);
      for (const statement of node.code) {
        traverseAst(statement, node, callback);
      }
      break;
    case "type declaration":
      traverseAst(node.typeNode, node, callback);
      break;
    case "variable declaration":
      if (node.typeNode) traverseAst(node.typeNode, node, callback);
      traverseAst(node.initializer, node, callback);
      break;
    case "if":
      traverseAst(node.condition, node, callback);
      for (const statement of node.trueBlock) {
        traverseAst(statement, node, callback);
      }
      for (const elseIf of node.elseIfs) {
        traverseAst(elseIf, node, callback);
      }
      for (const statement of node.falseBlock) {
        traverseAst(statement, node, callback);
      }
      break;
    case "while":
      traverseAst(node.condition, node, callback);
      for (const statement of node.block) {
        traverseAst(statement, node, callback);
      }
      break;
    case "loop variable":
      break;
    case "for each":
      traverseAst(node.list, node, callback);
      for (const statement of node.block) {
        traverseAst(statement, node, callback);
      }
      break;
    case "for":
      traverseAst(node.from, node, callback);
      traverseAst(node.to, node, callback);
      if (node.step) traverseAst(node.step, node, callback);
      for (const statement of node.block) {
        traverseAst(statement, node, callback);
      }
      break;
    case "do":
      for (const statement of node.block) {
        traverseAst(statement, node, callback);
      }
      traverseAst(node.condition, node, callback);
      break;
    case "continue":
      break;
    case "break":
      break;
    case "return":
      if (node.expression) traverseAst(node.expression, node, callback);
      break;
    case "ternary operator":
      traverseAst(node.condition, node, callback);
      traverseAst(node.trueExpression, node, callback);
      traverseAst(node.falseExpression, node, callback);
      break;
    case "binary operator":
      traverseAst(node.leftExpression, node, callback);
      traverseAst(node.rightExpression, node, callback);
      break;
    case "unary operator":
      traverseAst(node.expression, node, callback);
      break;
    case "is operator":
    case "as operator":
      traverseAst(node.leftExpression, node, callback);
      traverseAst(node.typeNode, node, callback);
      break;
    case "list literal":
      for (const element of node.elements) {
        traverseAst(element, node, callback);
      }
      if (node.typeNode) traverseAst(node.typeNode, node, callback);
      break;
    case "map literal":
      for (let i = 0; i < node.values.length; i++) {
        traverseAst(node.values[i], node, callback);
      }
      if (node.typeNode) traverseAst(node.typeNode, node, callback);
      break;
    case "record literal":
      for (let i = 0; i < node.fieldValues.length; i++) {
        traverseAst(node.fieldValues[i], node, callback);
      }
      break;
    case "function literal":
      for (const param of node.parameters) {
        traverseAst(param, node, callback);
      }
      if (node.returnType) traverseAst(node.returnType, node, callback);
      for (const statement of node.code) {
        traverseAst(statement, node, callback);
      }
      break;
    case "variable access":
      break;
    case "member access":
      traverseAst(node.object, node, callback);
      break;
    case "map or list access":
      traverseAst(node.target, node, callback);
      traverseAst(node.keyOrIndex, node, callback);
      break;
    case "function call":
      traverseAst(node.target, node, callback);
      for (const arg of node.args) {
        traverseAst(arg, node, callback);
      }
      break;
    case "method call":
      traverseAst(node.target, node, callback);
      for (const arg of node.args) {
        traverseAst(arg, node, callback);
      }
      break;
    default:
      assertNever(node);
  }
}

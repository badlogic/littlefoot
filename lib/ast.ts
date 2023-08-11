import { languages } from "monaco-editor";
import { LittleFootError } from "./error";
import { SourceLocation } from "./source";
import { BoolToken, IdentifierToken, NothingToken, NumberToken, OperatorToken, StringToken, Token } from "./tokenizer";
import { AnyType, Type, UnionType, UnknownType, isGeneric, rawType } from "./types";

export type AstNode = ImportNode | ImportedNameNode | InternalNode | TopLevelNode;

export type TopLevelNode = FunctionNode | TypeNode | StatementNode;

export type InternalNode = NameAndTypeNode | LoopVariable | NoopNode | TypeSpecifierNode;

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
  | ExpressionNode
  | UnboxedVariableNode;

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
  | MethodCallNode
  // These are "artificial" nodes generated during type checking
  | IncompleteExpressionNode
  | NumericWideningNode
  | UnionBoxingNode
  | UnionUnboxingNode
  | ExpressionPreambleNode;

export abstract class BaseAstNode {
  protected _type: Type = UnknownType;

  constructor(public readonly location: SourceLocation) {}

  set type(type: Type) {
    this._type = type;
  }

  get type(): Type {
    return this._type;
  }

  abstract copy(): BaseAstNode;
}

export class TypeReferenceNode extends BaseAstNode {
  public readonly kind: "type reference" = "type reference";
  constructor(
    public readonly name: IdentifierToken,
    public readonly genericTypeBindings: TypeSpecifierNode[],
    private readonly closingBracket: OperatorToken | null
  ) {
    super(SourceLocation.from(name.location, closingBracket ? closingBracket.location : name.location));
  }

  copy(): TypeReferenceNode {
    return new TypeReferenceNode(this.name, this.genericTypeBindings.map((binding) => binding.copy()) as TypeSpecifierNode[], this.closingBracket);
  }
}

export class ListTypeNode extends BaseAstNode {
  public readonly kind: "list type" = "list type";

  constructor(
    private readonly openingBracket: OperatorToken,
    public readonly elementType: TypeSpecifierNode,
    private readonly closingBracket: OperatorToken
  ) {
    super(SourceLocation.from(openingBracket.location, closingBracket.location));
  }

  copy(): ListTypeNode {
    return new ListTypeNode(this.openingBracket, this.elementType.copy() as TypeSpecifierNode, this.closingBracket);
  }
}

export class MapTypeNode extends BaseAstNode {
  public readonly kind: "map type" = "map type";
  constructor(
    private readonly openingCurly: OperatorToken,
    public readonly valueType: TypeSpecifierNode,
    private readonly closingCurly: OperatorToken
  ) {
    super(SourceLocation.from(openingCurly.location, closingCurly.location));
  }

  copy(): MapTypeNode {
    return new MapTypeNode(this.openingCurly, this.valueType.copy() as TypeSpecifierNode, this.closingCurly);
  }
}

export class FunctionTypeNode extends BaseAstNode {
  public readonly kind: "function type" = "function type";
  constructor(
    private readonly firstToken: Token,
    public readonly parameters: NameAndTypeNode[],
    public returnType: TypeSpecifierNode | null,
    private lastLocation: SourceLocation
  ) {
    super(SourceLocation.from(firstToken.location, lastLocation));
  }

  copy(): FunctionTypeNode {
    return new FunctionTypeNode(
      this.firstToken,
      this.parameters.map((param) => new NameAndTypeNode(param.name, param.typeNode.copy() as TypeSpecifierNode)),
      this.returnType ? (this.returnType.copy() as TypeSpecifierNode) : null,
      this.lastLocation
    );
  }
}

export class RecordTypeNode extends BaseAstNode {
  public readonly kind: "record type" = "record type";
  constructor(private readonly firstToken: Token, public readonly fields: NameAndTypeNode[], private readonly lastToken: Token) {
    super(SourceLocation.from(firstToken.location, lastToken.location));
  }

  copy(): RecordTypeNode {
    return new RecordTypeNode(
      this.firstToken,
      this.fields.map((field) => field.copy()),
      this.lastToken
    );
  }
}

export class UnionTypeNode extends BaseAstNode {
  public readonly kind: "union type" = "union type";
  constructor(public readonly unionTypes: TypeSpecifierNode[]) {
    super(SourceLocation.from(unionTypes[0].location, unionTypes[unionTypes.length - 1].location));
  }

  copy(): UnionTypeNode {
    return new UnionTypeNode(this.unionTypes.map((unionType) => unionType.copy()) as TypeSpecifierNode[]);
  }
}

export class MixinTypeNode extends BaseAstNode {
  public readonly kind: "mixin type" = "mixin type";
  constructor(public readonly mixinTypes: TypeSpecifierNode[]) {
    super(SourceLocation.from(mixinTypes[0].location, mixinTypes[mixinTypes.length - 1].location));
  }

  copy(): MixinTypeNode {
    return new MixinTypeNode(this.mixinTypes.map((mixinType) => mixinType.copy()) as TypeSpecifierNode[]);
  }
}

export class TypeNode extends BaseAstNode {
  public readonly kind: "type declaration" = "type declaration";

  constructor(
    private readonly firstToken: Token,
    public readonly name: IdentifierToken,
    public readonly genericTypeNames: IdentifierToken[],
    public readonly typeNode: TypeSpecifierNode,
    public readonly exported: boolean
  ) {
    super(SourceLocation.from(firstToken.location, typeNode.location));
  }

  copy(): TypeNode {
    return new TypeNode(this.firstToken, this.name, this.genericTypeNames, this.typeNode.copy() as TypeSpecifierNode, this.exported);
  }
}

export class NameAndTypeNode extends BaseAstNode {
  public readonly kind: "name and type" = "name and type";
  constructor(public readonly name: IdentifierToken, public readonly typeNode: TypeSpecifierNode) {
    super(SourceLocation.from(name.location, typeNode.location));
  }

  copy(): NameAndTypeNode {
    return new NameAndTypeNode(this.name, this.typeNode.copy() as TypeSpecifierNode);
  }
}

export class ImportedNameNode extends BaseAstNode {
  public readonly kind: "imported name" = "imported name";
  constructor(public readonly name: IdentifierToken, public readonly alias: IdentifierToken | null) {
    super(SourceLocation.from(name.location, alias ? alias.location : name.location));
  }

  copy(): ImportedNameNode {
    return new ImportedNameNode(this.name, this.alias);
  }
}

export class ImportNode extends BaseAstNode {
  public readonly kind: "import" = "import";

  constructor(private readonly firstToken: Token, public readonly importedNames: ImportedNameNode[], public readonly path: StringToken) {
    super(SourceLocation.from(firstToken.location, path.location));
  }

  copy(): ImportNode {
    return new ImportNode(
      this.firstToken,
      this.importedNames.map((importedName) => importedName.copy()),
      this.path
    );
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

  copy(): FunctionNode {
    return new FunctionNode(
      this.location,
      this.name,
      this.genericTypeNames,
      this.parameters.map((param) => param.copy()),
      this.returnType ? (this.returnType.copy() as TypeSpecifierNode) : null,
      this.code.map((stmt) => stmt.copy() as StatementNode),
      this.exported,
      this.external
    );
  }
}

export class VariableNode extends BaseAstNode {
  public kind: "variable declaration" = "variable declaration";
  constructor(
    private readonly firstToken: Token,
    public readonly name: IdentifierToken,
    public typeNode: TypeSpecifierNode | null,
    public initializer: ExpressionNode,
    public readonly exported: boolean,
    public readonly constant: boolean,
    public moduleVariable = false
  ) {
    super(SourceLocation.from(firstToken.location, initializer.location));
  }

  copy(): VariableNode {
    return new VariableNode(
      this.firstToken,
      this.name,
      this.typeNode ? (this.typeNode.copy() as TypeSpecifierNode) : null,
      this.initializer.copy() as ExpressionNode,
      this.exported,
      this.constant,
      this.moduleVariable
    );
  }
}

export class IfNode extends BaseAstNode {
  public readonly kind: "if" = "if";
  constructor(
    private readonly firstToken: Token,
    public readonly condition: ExpressionNode,
    public readonly trueBlock: StatementNode[],
    public readonly elseIfs: IfNode[],
    public readonly falseBlock: StatementNode[],
    public readonly lastLocation: SourceLocation
  ) {
    super(SourceLocation.from(firstToken.location, lastLocation));
  }

  copy(): IfNode {
    return new IfNode(
      this.firstToken,
      this.condition.copy() as ExpressionNode,
      this.trueBlock.map((stmt) => stmt.copy() as StatementNode),
      this.elseIfs.map((elseif) => elseif.copy() as IfNode),
      this.falseBlock.map((stmt) => stmt.copy() as StatementNode),
      this.lastLocation
    );
  }
}

export class WhileNode extends BaseAstNode {
  public readonly kind: "while" = "while";
  constructor(
    private readonly firstToken: Token,
    public readonly condition: ExpressionNode,
    public readonly block: StatementNode[],
    public readonly lastToken: Token
  ) {
    super(SourceLocation.from(firstToken.location, lastToken.location));
  }

  copy(): WhileNode {
    return new WhileNode(
      this.firstToken,
      this.condition.copy() as ExpressionNode,
      this.block.map((stmt) => stmt.copy() as StatementNode),
      this.lastToken
    );
  }
}

export class LoopVariable extends BaseAstNode {
  public readonly kind: "loop variable" = "loop variable";
  constructor(public readonly name: IdentifierToken, public readonly typeSpecifier: TypeSpecifierNode | null) {
    super(name.location);
  }

  copy(): LoopVariable {
    return new LoopVariable(this.name, this.typeSpecifier ? (this.typeSpecifier.copy() as TypeSpecifierNode) : null);
  }
}

export class ForNode extends BaseAstNode {
  public readonly kind: "for" = "for";
  constructor(
    private readonly firstToken: Token,
    public readonly loopVariable: LoopVariable,
    public from: ExpressionNode,
    public to: ExpressionNode,
    public step: ExpressionNode | null,
    public readonly block: StatementNode[],
    private readonly lastToken: Token
  ) {
    super(SourceLocation.from(firstToken.location, lastToken.location));
  }

  copy(): ForNode {
    return new ForNode(
      this.firstToken,
      this.loopVariable.copy(),
      this.from.copy() as ExpressionNode,
      this.to.copy() as ExpressionNode,
      this.step ? (this.step.copy() as ExpressionNode) : null,
      this.block.map((stmt) => stmt.copy() as StatementNode),
      this.lastToken
    );
  }
}

export class ForEachNode extends BaseAstNode {
  public readonly kind: "for each" = "for each";
  constructor(
    private readonly firstToken: Token,
    public readonly loopVariable: LoopVariable,
    public readonly list: ExpressionNode,
    public readonly block: StatementNode[],
    private readonly lastToken: Token
  ) {
    super(SourceLocation.from(firstToken.location, lastToken.location));
  }

  copy(): ForEachNode {
    return new ForEachNode(
      this.firstToken,
      this.loopVariable.copy(),
      this.list.copy() as ExpressionNode,
      this.block.map((stmt) => stmt.copy() as StatementNode),
      this.lastToken
    );
  }
}

export class DoNode extends BaseAstNode {
  public readonly kind: "do" = "do";
  constructor(private readonly firstToken: Token, public readonly condition: ExpressionNode, public readonly block: StatementNode[]) {
    super(SourceLocation.from(firstToken.location, condition.location));
  }

  copy(): DoNode {
    return new DoNode(
      this.firstToken,
      this.condition.copy() as ExpressionNode,
      this.block.map((stmt) => stmt.copy() as StatementNode)
    );
  }
}

export class ContinueNode extends BaseAstNode {
  public readonly kind: "continue" = "continue";
  constructor(private readonly token: Token) {
    super(token.location);
  }

  copy(): BaseAstNode {
    return new ContinueNode(this.token);
  }
}

export class BreakNode extends BaseAstNode {
  public readonly kind: "break" = "break";
  constructor(private readonly token: Token) {
    super(token.location);
  }

  copy(): BaseAstNode {
    return new BreakNode(this.token);
  }
}

export class ReturnNode extends BaseAstNode {
  public readonly kind: "return" = "return";
  constructor(private readonly firstToken: Token, public expression: ExpressionNode | null) {
    super(SourceLocation.from(firstToken.location, expression ? expression.location : firstToken.location));
  }

  copy(): ReturnNode {
    return new ReturnNode(this.firstToken, this.expression ? (this.expression.copy() as ExpressionNode) : null);
  }
}

export class TernaryOperatorNode extends BaseAstNode {
  public readonly kind: "ternary operator" = "ternary operator";
  constructor(public condition: ExpressionNode, public trueExpression: ExpressionNode, public falseExpression: ExpressionNode) {
    super(SourceLocation.from(condition.location, falseExpression.location));
  }

  copy(): TernaryOperatorNode {
    return new TernaryOperatorNode(
      this.condition.copy() as ExpressionNode,
      this.trueExpression.copy() as ExpressionNode,
      this.falseExpression.copy() as ExpressionNode
    );
  }
}

export class BinaryOperatorNode extends BaseAstNode {
  public readonly kind: "binary operator" = "binary operator";
  constructor(public leftExpression: ExpressionNode, public readonly operator: OperatorToken, public rightExpression: ExpressionNode) {
    super(SourceLocation.from(leftExpression.location, rightExpression.location));
  }

  copy(): BinaryOperatorNode {
    return new BinaryOperatorNode(this.leftExpression.copy() as ExpressionNode, this.operator, this.rightExpression.copy() as ExpressionNode);
  }
}

export class UnaryOperatorNode extends BaseAstNode {
  public readonly kind: "unary operator" = "unary operator";
  constructor(public readonly operator: OperatorToken, public expression: ExpressionNode) {
    super(SourceLocation.from(operator.location, expression.location));
  }

  copy(): UnaryOperatorNode {
    return new UnaryOperatorNode(this.operator, this.expression.copy() as ExpressionNode);
  }
}

export class IsOperatorNode extends BaseAstNode {
  public readonly kind: "is operator" = "is operator";
  constructor(
    public readonly leftExpression: ExpressionNode,
    public readonly variableName: IdentifierToken | null,
    public readonly typeNode: TypeSpecifierNode
  ) {
    super(SourceLocation.from(leftExpression.location, typeNode.location));
  }

  copy(): IsOperatorNode {
    return new IsOperatorNode(this.leftExpression.copy() as ExpressionNode, this.variableName, this.typeNode.copy() as TypeSpecifierNode);
  }
}

export class AsOperatorNode extends BaseAstNode {
  public readonly kind: "as operator" = "as operator";
  constructor(public leftExpression: ExpressionNode, public readonly typeNode: TypeSpecifierNode) {
    super(SourceLocation.from(leftExpression.location, typeNode.location));
  }

  copy(): AsOperatorNode {
    return new AsOperatorNode(this.leftExpression.copy() as ExpressionNode, this.typeNode.copy() as TypeSpecifierNode);
  }
}

export class StringLiteralNode extends BaseAstNode {
  public readonly kind: "string literal" = "string literal";
  constructor(public readonly token: StringToken) {
    super(token.location);
  }

  copy(): StringLiteralNode {
    return new StringLiteralNode(this.token);
  }
}

export class NumberLiteralNode extends BaseAstNode {
  public readonly kind: "number literal" = "number literal";
  constructor(public readonly token: NumberToken) {
    super(token.location);
  }

  copy(): NumberLiteralNode {
    return new NumberLiteralNode(this.token);
  }
}

export class BooleanLiteralNode extends BaseAstNode {
  public readonly kind: "boolean literal" = "boolean literal";
  constructor(public readonly token: BoolToken) {
    super(token.location);
  }

  copy(): BooleanLiteralNode {
    return new BooleanLiteralNode(this.token);
  }
}

export class NothingLiteralNode extends BaseAstNode {
  public readonly kind: "nothing literal" = "nothing literal";
  constructor(public readonly token: NothingToken) {
    super(token.location);
  }

  copy(): NothingLiteralNode {
    return new NothingLiteralNode(this.token);
  }
}

export class ListLiteralNode extends BaseAstNode {
  public readonly kind: "list literal" = "list literal";
  constructor(
    private readonly firstToken: Token,
    public readonly elements: ExpressionNode[],
    public readonly typeNode: TypeSpecifierNode | null,
    private readonly lastToken: Token
  ) {
    super(SourceLocation.from(firstToken.location, lastToken.location));
  }

  copy(): ListLiteralNode {
    return new ListLiteralNode(
      this.firstToken,
      this.elements.map((el) => el.copy() as ExpressionNode),
      this.typeNode ? (this.typeNode.copy() as TypeSpecifierNode) : null,
      this.lastToken
    );
  }
}

export class MapLiteralNode extends BaseAstNode {
  public readonly kind: "map literal" = "map literal";
  constructor(
    private readonly firstToken: Token,
    public readonly keys: StringToken[],
    public readonly values: ExpressionNode[],
    public readonly typeNode: TypeSpecifierNode | null,
    private readonly lastToken: Token
  ) {
    super(SourceLocation.from(firstToken.location, lastToken.location));
  }

  copy(): MapLiteralNode {
    return new MapLiteralNode(
      this.firstToken,
      this.keys,
      this.values.map((el) => el.copy() as ExpressionNode),
      this.typeNode ? (this.typeNode.copy() as TypeSpecifierNode) : null,
      this.lastToken
    );
  }
}

export class RecordLiteralNode extends BaseAstNode {
  public readonly kind: "record literal" = "record literal";
  constructor(
    private readonly firstToken: Token,
    public readonly fieldNames: IdentifierToken[],
    public readonly fieldValues: ExpressionNode[],
    private readonly lastToken: Token
  ) {
    super(SourceLocation.from(firstToken.location, lastToken.location));
  }

  copy(): RecordLiteralNode {
    return new RecordLiteralNode(
      this.firstToken,
      this.fieldNames,
      this.fieldValues.map((value) => value.copy() as ExpressionNode),
      this.lastToken
    );
  }
}

export class FunctionLiteralNode extends BaseAstNode {
  public readonly kind: "function literal" = "function literal";
  constructor(
    private readonly firstToken: Token,
    public readonly parameters: NameAndTypeNode[],
    public returnType: TypeSpecifierNode | null,
    public readonly code: StatementNode[],
    private readonly lastLocation: SourceLocation
  ) {
    super(SourceLocation.from(firstToken.location, lastLocation));
  }

  copy(): FunctionLiteralNode {
    return new FunctionLiteralNode(
      this.firstToken,
      this.parameters.map((param) => param.copy()),
      this.returnType ? (this.returnType.copy() as TypeSpecifierNode) : null,
      this.code.map((stmt) => stmt.copy() as StatementNode),
      this.lastLocation
    );
  }
}

export class VariableAccessNode extends BaseAstNode {
  public readonly kind: "variable access" = "variable access";
  constructor(public readonly name: IdentifierToken) {
    super(name.location);
  }

  copy(): VariableAccessNode {
    return new VariableAccessNode(this.name);
  }
}

export class MemberAccessNode extends BaseAstNode {
  public readonly kind: "member access" = "member access";
  constructor(public readonly object: ExpressionNode, public readonly member: IdentifierToken) {
    super(SourceLocation.from(object.location, member.location));
  }

  copy(): MemberAccessNode {
    return new MemberAccessNode(this.object.copy() as ExpressionNode, this.member);
  }
}

export class MapOrListAccessNode extends BaseAstNode {
  public readonly kind: "map or list access" = "map or list access";
  constructor(
    private readonly openingBracket: Token,
    public readonly target: ExpressionNode,
    public readonly keyOrIndex: ExpressionNode,
    private readonly lastToken: Token
  ) {
    super(SourceLocation.from(openingBracket.location, lastToken.location));
  }

  copy(): MapOrListAccessNode {
    return new MapOrListAccessNode(
      this.openingBracket,
      this.target.copy() as ExpressionNode,
      this.keyOrIndex.copy() as ExpressionNode,
      this.lastToken
    );
  }
}

export class FunctionCallNode extends BaseAstNode {
  public readonly kind: "function call" = "function call";
  constructor(
    public readonly target: VariableAccessNode | MapOrListAccessNode | FunctionLiteralNode,
    public readonly args: ExpressionNode[],
    private readonly lastToken: Token
  ) {
    super(SourceLocation.from(target.location, lastToken.location));
  }

  copy(): FunctionCallNode {
    return new FunctionCallNode(
      this.target.copy() as VariableAccessNode | MapOrListAccessNode | FunctionLiteralNode,
      this.args.map((arg) => arg.copy() as ExpressionNode),
      this.lastToken
    );
  }
}

export class MethodCallNode extends BaseAstNode {
  public readonly kind: "method call" = "method call";
  constructor(public readonly target: MemberAccessNode, public readonly args: ExpressionNode[], public readonly lastToken: Token) {
    super(SourceLocation.from(target.location, lastToken.location));
  }

  copy(): MethodCallNode {
    return new MethodCallNode(
      this.target.copy(),
      this.args.map((arg) => arg.copy() as ExpressionNode),
      this.lastToken
    );
  }
}

export class IncompleteExpressionNode extends BaseAstNode {
  public readonly kind: "incomplete expression" = "incomplete expression";
  constructor(public readonly expression: ExpressionNode, public readonly error: LittleFootError) {
    super(expression.location);
  }

  copy(): IncompleteExpressionNode {
    return new IncompleteExpressionNode(this.expression.copy() as ExpressionNode, this.error);
  }
}

export class NumericWideningNode extends BaseAstNode {
  public readonly kind: "numeric widening" = "numeric widening";
  constructor(public readonly expression: AstNode, private readonly narrowToType: Type) {
    super(expression.location);
    this._type = narrowToType;
  }

  copy(): NumericWideningNode {
    return new NumericWideningNode(this.expression.copy() as ExpressionNode, this.narrowToType);
  }
}

export class UnionBoxingNode extends BaseAstNode {
  public readonly kind: "union boxing" = "union boxing";
  constructor(public readonly expression: ExpressionNode, private readonly unionType: Type) {
    super(expression.location);
    this._type = unionType;
    if (rawType(unionType).kind != "union" && rawType(expression.type) != AnyType) {
      throw new LittleFootError(expression.location, `Internal error: created union boxing node with non-union type ${unionType.signature}.`);
    }
  }

  copy(): UnionBoxingNode {
    return new UnionBoxingNode(this.expression.copy() as ExpressionNode, this.unionType);
  }
}

export class UnboxedVariableNode extends BaseAstNode {
  public readonly kind: "unboxed variable declaration" = "unboxed variable declaration";

  constructor(public readonly name: IdentifierToken, public readonly unboxedValue: UnionUnboxingNode) {
    super(name.location);
    this._type = unboxedValue.type;
  }

  copy(): NoopNode {
    // This is only called when instantiating a generic function. In that case, the new type checker
    // pass will add a new unboxed variable node, which is why we turn this node into a noop.
    return new NoopNode(this.name.location);
  }
}

export class UnionUnboxingNode extends BaseAstNode {
  public readonly kind: "union unboxing" = "union unboxing";
  constructor(public readonly expression: ExpressionNode, unboxedType: Type) {
    super(expression.location);
    this._type = unboxedType;
    if (rawType(expression.type).kind != "union" && rawType(expression.type) != AnyType) {
      throw new LittleFootError(
        expression.location,
        `Internal error: created union unboxing node with non-union expression ${expression.type.signature}.`
      );
    }
  }

  copy(): ExpressionNode {
    throw new LittleFootError(this.location, "Internal error: an union unboxing node should never be copied");
  }
}

export class ExpressionPreambleNode extends BaseAstNode {
  public readonly kind: "expression preamble" = "expression preamble";
  constructor(public readonly preamble: StatementNode[], public readonly expression: ExpressionNode) {
    super(expression.location);
    this._type = expression.type;
  }

  copy(): ExpressionNode {
    throw new LittleFootError(this.location, "Internal error: an expression preamble node should never be copied");
  }
}

export class NoopNode extends BaseAstNode {
  public readonly kind: "noop" = "noop";

  constructor(location: SourceLocation) {
    super(location);
  }

  copy(): BaseAstNode {
    return this;
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
    case "incomplete expression":
      traverseAst(node.expression, node, callback);
      break;
    case "numeric widening":
      traverseAst(node.expression, node, callback);
      break;
    case "union boxing":
      traverseAst(node.expression, node, callback);
      break;
    case "unboxed variable declaration":
      traverseAst(node.unboxedValue, node, callback);
      break;
    case "union unboxing":
      traverseAst(node.expression, node, callback);
      break;
    case "expression preamble":
      for (const statement of node.preamble) {
        traverseAst(statement, node, callback);
      }
      traverseAst(node.expression, node, callback);
      break;
    case "noop":
      break;
    default:
      assertNever(node);
  }
}

export function unbox(node: ExpressionNode): ExpressionNode {
  while (node.kind == "union boxing") {
    node = node.expression;
  }
  return node;
}

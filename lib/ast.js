export class AstNode {}

export class CommentNode extends AstNode {
  constructor(lines) {
    super();
    this.nodeType = "comment";
    this.lines = lines;
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

export class PlainTypeNode extends AstNode {
  constructor(typeName) {
    super();
    this.nodeType = "plain type";
    this.typeName = typeName;
  }
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

export class TypeDeclarationNode extends AstNode {
  constructor(name, fields) {
    super();
    this.nodeType = "type declaration";
    this.name = name;
    this.fields = fields;
  }
}

export class NameAndTypeNode extends AstNode {
  constructor(name, type) {
    super();
    this.nodeType = "name and type";
    this.name = name;
    this.type = type;
  }
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

export class WhileNode extends AstNode {
  constructor(condition, block) {
    super();
    this.nodeType = "while";
    this.condition = condition;
    this.block = block;
  }
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

export class DoNode extends AstNode {
  constructor(condition, block) {
    super();
    this.nodeType = "do";
    this.condition = condition;
    this.block = block;
  }
}

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

export class UnaryOperatorNode extends AstNode {
  constructor(operator, expression) {
    super();
    this.nodeType = "unary operator";
    this.operator = operator;
    this.expression = expression;
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

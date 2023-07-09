import { AstNode, TypeNode } from "./ast";
import { LittleFootError } from "./error";
import {
  ArrayType,
  BooleanType,
  FunctionType,
  MapType,
  NameAndType,
  NamedFunction,
  NamedType,
  NothingType,
  NumberType,
  StringType,
  TupleType,
  Types,
  UnionType,
  UnknownType,
} from "./types";

function assertNever(x: never) {
  throw new Error("Unexpected object: " + x);
}

export function checkTypes(ast: AstNode[], errors: LittleFootError[], types: Types) {
  // Gather named type nodes. These are named types that other
  // types may refer to. Create placeholder types.
  const namedTypes: TypeNode[] = ast.filter((node) => node.kind == "type declaration") as TypeNode[];
  for (const type of namedTypes) {
    if (types.has(type.name.value)) {
      errors.push(
        new LittleFootError(
          type.name.start,
          type.name.end,
          type.firstToken.source,
          `Can not use '${type.name.value}' as a type name, as a built-in type with that name exists.`
        )
      );
    }
    type.type = new NamedType(type.name.value, UnknownType, type);
  }

  // Deduplicate named types. Can't use types.add()/has() as the signature
  // is incomplete at this point.
  const namedTypesLookup = new Map<String, TypeNode>();
  for (const type of namedTypes) {
    if (namedTypesLookup.has(type.name.value)) {
      const otherType = namedTypesLookup.get(type.name.value)!;
      const otherTypeLineIndex = otherType.name.source.indicesToLines(otherType.name.start, otherType.name.end)[0].index;
      errors.push(
        new LittleFootError(
          type.name.start,
          type.name.end,
          type.firstToken.source,
          `Duplicate type '${type.name.value}', first defined in ${otherType.firstToken.source}:${otherTypeLineIndex}`
        )
      );
    } else {
      namedTypesLookup.set(type.name.value, type);
    }
  }
  if (errors.length > 0) return;

  // Replace placeholder types with the real thing.
  for (const type of namedTypes) {
    types.add(type.type as NamedType);
  }
  for (const type of namedTypes) {
    try {
      checkNodeTypes(type, types);
    } catch (e) {
      if (e instanceof LittleFootError) errors.push(e);
      else errors.push(new LittleFootError(0, 1, type.firstToken.source, "Internal error: " + (e as any).message + "\n" + (e as any).stack));
      return;
    }
  }

  for (const node of ast) {
    // FIXME recover in case a statement or expression throws an error and type check the remainder of the AST if possible
    // This should work for errors within functions. For top-level statements, stop if a var declaration fails.
    // checkNodeTypes(node, types);
  }
}

export function checkNodeTypes(node: AstNode, types: Types) {
  switch (node.kind) {
    case "nothing": {
      node.type = NothingType;
      break;
    }
    case "boolean": {
      node.type = BooleanType;
      break;
    }
    case "number": {
      node.type = NumberType;
      break;
    }
    case "string": {
      node.type = StringType;
      break;
    }
    case "array type": {
      checkNodeTypes(node.elementType, types);
      node.type = new ArrayType(node.elementType.type);
      break;
    }
    case "map type": {
      checkNodeTypes(node.valueType, types);
      node.type = new MapType(node.valueType.type);
      break;
    }
    case "function type": {
      if (node.returnType) checkNodeTypes(node.returnType, types);
      for (const parameter of node.parameters) {
        checkNodeTypes(parameter, types);
      }
      node.type = new FunctionType(
        node.parameters.map((parameter) => parameter.type as NameAndType),
        node.returnType ? node.returnType.type : NothingType
      );
      break;
    }
    case "tuple type": {
      for (const field of node.fields) {
        checkNodeTypes(field, types);
      }
      node.type = new TupleType(node.fields.map((field) => new NameAndType(field.name.value, field.type)));
      break;
    }
    case "union type": {
      for (const type of node.unionTypes) {
        checkNodeTypes(type, types);
      }
      node.type = new UnionType(node.unionTypes.map((type) => type.type));
      break;
    }
    case "mixin type": {
      for (const type of node.mixinTypes) {
        checkNodeTypes(type, types);
      }
      // FIXME merge fields of mixins and generate a new TupleType
      throw Error("not implemented");
    }
    case "named type": {
      if (!types.has(node.name.value)) {
        throw new LittleFootError(node.start, node.end, node.firstToken.source, `Could not find type '${node.name.value}'`);
      }
      node.type = types.get(node.name.value)!;
      break;
    }
    case "name and type": {
      checkNodeTypes(node.typeNode, types);
      node.type = node.typeNode.type;
      break;
    }
    case "function declaration": {
      for (const parameter of node.parameters) {
        checkNodeTypes(parameter, types);
      }
      if (node.returnType) checkNodeTypes(node.returnType, types);
      const functionType = new FunctionType(
        node.parameters.map((parameter) => new NameAndType(parameter.name.value, parameter.type)),
        node.returnType ? node.returnType.type : NothingType
      );
      node.type = functionType;
      const namedFunction = new NamedFunction(node.name.value, functionType, node);
      if (types.has(namedFunction.signature)) {
        const otherType = types.get(namedFunction.signature)! as NamedFunction;
        const otherTypeLineIndex = otherType.node.name.source.indicesToLines(otherType.node.name.start, otherType.node.name.end)[0].index;
        throw new LittleFootError(
          node.name.start,
          node.name.end,
          node.firstToken.source,
          `Duplicate function '${node.name.value}', first defined in ${otherType.node.firstToken.source}:${otherTypeLineIndex}`
        );
      }
      types.add(namedFunction);
      for (const statement of node.code) {
        checkNodeTypes(statement, types);
      }
      break;
    }
    case "type declaration": {
      checkNodeTypes(node.typeNode, types);
      if (!node.type) {
        throw new LittleFootError(
          node.name.start,
          node.name.end,
          node.firstToken.source,
          `Internal compiler error: named type ${node.name.value} should have a type set.`
        );
      }
      (node.type as NamedType).type = node.typeNode.type;
      break;
    }
    case "variable declaration":
      throw Error("not implemented");
    case "if":
      throw Error("not implemented");
    case "while":
      throw Error("not implemented");
    case "for each":
      throw Error("not implemented");
    case "for":
      throw Error("not implemented");
    case "do":
      throw Error("not implemented");
    case "continue":
      throw Error("not implemented");
    case "break":
      throw Error("not implemented");
    case "return":
      throw Error("not implemented");
    case "ternary operator":
      throw Error("not implemented");
    case "binary operator":
      throw Error("not implemented");
    case "unary operator":
      throw Error("not implemented");
    case "is operator":
      throw Error("not implemented");
    case "array literal":
    case "map literal":
    case "tuple literal":
    case "function literal":
    case "variable access":
    case "member access":
    case "map or array access":
    case "function call":
    case "method call":
      break;
    default:
      assertNever(node);
  }
}

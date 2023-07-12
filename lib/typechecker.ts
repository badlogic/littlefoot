import { AstNode, TypeNode, TypeSpecifierNode } from "./ast";
import { CompilerContext } from "./compiler";
import { LittleFootError } from "./error";
import { SourceLocation } from "./source";
// prettier-ignore
import { BooleanType, FunctionType, ListType, MapType, NameAndType, NamedFunction, NamedType, NothingType, NumberType, RecordType, ResolvingTypeMarker, StringType, UnionType, UnknownType } from "./types";

function assertNever(x: never) {
  throw new Error("Unexpected object: " + x);
}

export function checkTypes(ast: AstNode[], context: CompilerContext) {
  const { errors, types } = context;

  // Gather named type nodes. These are named types that other
  // types may refer to. Create placeholder types.
  const namedTypes: TypeNode[] = ast.filter((node) => node.kind == "type declaration") as TypeNode[];
  for (const type of namedTypes) {
    if (types.has(type.name.value)) {
      errors.push(
        new LittleFootError(type.name.location, `Can not use '${type.name.value}' as a type name, as a built-in type with that name exists.`)
      );
    }
    type.type = new NamedType(type.name.value, UnknownType, type, type.location);
  }

  // Deduplicate named types. Can't use types.add()/has() as the signature
  // is incomplete at this point.
  const namedTypesLookup = new Map<String, TypeNode>();
  for (const type of namedTypes) {
    if (namedTypesLookup.has(type.name.value)) {
      const otherType = namedTypesLookup.get(type.name.value)!;
      const otherTypeLineIndex = otherType.location.source.indicesToLines(otherType.name.location.start, otherType.name.location.end)[0].index;
      errors.push(
        new LittleFootError(
          type.name.location,
          `Duplicate type '${type.name.value}', first defined in ${otherType.location.source.path}:${otherTypeLineIndex}`
        )
      );
    } else {
      namedTypesLookup.set(type.name.value, type);
    }
  }
  if (errors.length > 0) return;

  // Replace placeholder types with the real thing. Detects circular types.
  for (const type of namedTypes) {
    types.add(type.type as NamedType);
  }
  for (const type of namedTypes) {
    try {
      checkNodeTypes(type, context);
    } catch (e) {
      if (e instanceof LittleFootError) errors.push(e);
      else
        errors.push(
          new LittleFootError(new SourceLocation(type.location.source, 0, 1), "Internal error: " + (e as any).message + "\n" + (e as any).stack)
        );
      return;
    }
  }
  if (errors.length > 0) return;

  for (const node of ast) {
    // FIXME recover in case a statement or expression throws an error and type check the remainder of the AST if possible
    // This should work for errors within functions. For top-level statements, stop if a var declaration fails.
    try {
      checkNodeTypes(node, context);
    } catch (e) {
      if (e instanceof LittleFootError) errors.push(e);
      else errors.push(new LittleFootError(node.location, "Internal error: " + (e as any).message + "\n" + (e as any).stack));
      return;
    }
  }
}

export function checkNodeTypes(node: AstNode, context: CompilerContext) {
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
    case "list type": {
      checkNodeTypes(node.elementType, context);
      node.type = new ListType(node.elementType.type);
      break;
    }
    case "map type": {
      checkNodeTypes(node.valueType, context);
      node.type = new MapType(node.valueType.type);
      break;
    }
    case "function type": {
      if (node.returnType) checkNodeTypes(node.returnType, context);
      for (const parameter of node.parameters) {
        checkNodeTypes(parameter, context);
      }
      node.type = new FunctionType(
        node.parameters.map((parameter) => parameter.type as NameAndType),
        node.returnType ? node.returnType.type : NothingType
      );
      break;
    }
    case "record type": {
      for (const field of node.fields) {
        checkNodeTypes(field, context);
      }
      node.type = new RecordType(node.fields.map((field) => new NameAndType(field.name.value, field.type)));
      break;
    }
    case "union type": {
      for (const type of node.unionTypes) {
        checkNodeTypes(type, context);
      }
      node.type = new UnionType(node.unionTypes.map((type) => type.type));
      break;
    }
    case "mixin type": {
      const seenFields = new Map<String, TypeSpecifierNode>();
      const fields = [];
      for (const type of node.mixinTypes) {
        checkNodeTypes(type, context);

        // Make sure the mixin type is a record
        if (!((type.type.kind == "named type" && type.type.type.kind == "record") || type.type.kind == "record")) {
          throw new LittleFootError(type.location, `All types in a mixin must be a record.`);
        }

        // Make sure the fields of the record are unique within the mixin
        const record = type.type.kind == "named type" ? (type.type.type as RecordType) : (type.type as RecordType);
        for (const field of record.fields) {
          if (!seenFields.has(field.name)) {
            seenFields.set(field.name, type);
            fields.push(field);
          } else {
            const otherType = seenFields.get(field.name)!;
            const previousType = otherType.location.text;
            throw new LittleFootError(type.location, `Field '${field.name}' of mixin type already defined in previous mixin type '${previousType}'.`);
          }
        }
      }
      node.type = new RecordType(fields);
      break;
    }
    case "type reference": {
      if (!context.types.has(node.name.value)) {
        throw new LittleFootError(node.location, `Could not find type '${node.name.value}'`);
      }
      const type = context.types.get(node.name.value)! as NamedType;
      // If we are in the type resolution phase, we might encounter types
      // that haven't been resolved yet in other type declarations. Resolve
      // them here. Set a marker type the first time, so we can detect
      // circular types.
      if (type.type == UnknownType) {
        type.type = ResolvingTypeMarker;
        checkNodeTypes(type.typeNode, context);
      }
      // Found a circular type
      if (type.type == ResolvingTypeMarker) {
        throw new LittleFootError(type.location, `Type '${type.name}' circularly references itself.`);
      }
      node.type = type;
      break;
    }
    case "name and type": {
      checkNodeTypes(node.typeNode, context);
      node.type = node.typeNode.type;
      break;
    }
    case "import":
      throw new Error("Not implemented");
    case "imported name": {
      break; // no-op, handled in "import" case above
    }
    case "function declaration": {
      for (const parameter of node.parameters) {
        checkNodeTypes(parameter, context);
      }

      if (node.returnType) checkNodeTypes(node.returnType, context);

      const functionType = new FunctionType(
        node.parameters.map((parameter) => new NameAndType(parameter.name.value, parameter.type)),
        node.returnType ? node.returnType.type : NothingType
      );
      node.type = functionType;
      const namedFunction = new NamedFunction(node.name.value, functionType, node.code, node.exported, node.external, node.location);
      if (context.types.has(namedFunction.signature)) {
        const otherType = context.types.get(namedFunction.signature)! as NamedFunction;
        const otherTypeLineIndex = otherType.location.source.indicesToLines(otherType.location.start, otherType.location.end)[0].index;
        throw new LittleFootError(
          node.name.location,
          `Duplicate function '${node.name.value}', first defined in ${otherType.location.source.path}:${otherTypeLineIndex}`
        );
      }
      context.types.add(namedFunction);
      for (const statement of node.code) {
        checkNodeTypes(statement, context);
      }
      break;
    }
    case "type declaration": {
      checkNodeTypes(node.typeNode, context);
      if (node.type == UnknownType) {
        throw new LittleFootError(node.name.location, `Internal compiler error: named type ${node.name.value} should have a type set.`);
      }
      (node.type as NamedType).type = node.typeNode.type;
      break;
    }
    case "variable declaration": {
      checkNodeTypes(node.initializer, context);
      if (node.typeNode) {
        checkNodeTypes(node.typeNode, context);
      } else {
        node.type = node.initializer.type;
      }
      break;
    }
    case "if": {
      checkNodeTypes(node.condition, context);
      if (node.condition.type != BooleanType) {
        throw new LittleFootError(node.condition.location, `If condition must have type boolean but has type '${node.condition.type.signature}'`);
      }
      break;
    }
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
    case "list literal":
      throw Error("not implemented");
    case "map literal":
      throw Error("not implemented");
    case "record literal":
      throw Error("not implemented");
    case "function literal":
      throw Error("not implemented");
    case "variable access":
      throw Error("not implemented");
    case "member access":
      throw Error("not implemented");
    case "map or list access":
      throw Error("not implemented");
    case "function call":
      throw Error("not implemented");
    case "method call":
      throw Error("not implemented");
    default:
      assertNever(node);
  }
}

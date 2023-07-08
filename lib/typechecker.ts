import { AstNodeType } from "./ast";

function assertNever(x: never) {
  throw new Error("Unexpected object: " + x);
}

export function assignTypesToNodes(ast: AstNodeType[]) {
  for (const node of ast) {
    assignTypesToNode(node);
  }
}

export function assignTypesToNode(node: AstNodeType) {
  switch (node.kind) {
    case "plain type":
      break;
    case "array type":
      break;
    case "map type":
      break;
    case "function type":
      break;
    case "tuple type":
      break;
    case "name and type":
      break;
    case "variable declaration":
      break;
    case "record declaration":
      break;
    case "if":
      break;
    case "while":
      break;
    case "for each":
      break;
    case "for":
      break;
    case "do":
      break;
    case "continue":
      break;
    case "break":
      break;
    case "return":
      break;
    case "ternary operator":
      break;
    case "binary operator":
      break;
    case "unary operator":
      break;
    case "is operator":
      break;
    case "string":
      break;
    case "number":
      break;
    case "boolean":
      break;
    case "nothing":
      break;
    case "array literal":
      break;
    case "map literal":
      break;
    case "tuple literal":
      break;
    case "function literal":
      break;
    case "variable access":
      break;
    case "member access":
      break;
    case "map or array access":
      break;
    case "function call":
      break;
    case "method call":
      break;
    default:
      return assertNever(node);
  }
}

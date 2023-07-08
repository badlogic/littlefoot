import { AstNode } from "./ast";
import { LittleFootError } from "./error";
import { Types } from "./types";

function assertNever(x: never) {
  throw new Error("Unexpected object: " + x);
}

export function checkTypes(ast: AstNode[], error: LittleFootError[], types: Types) {}

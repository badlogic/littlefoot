import { CancellationToken, Position, editor, languages } from "monaco-editor";
import { Module } from "../lib";
import { AstNode, traverseAst } from "../lib/ast";
import { UnknownType } from "../lib/types";

export class LittlefootCompletionsProvider implements languages.CompletionItemProvider {
  public modules: Map<string, Module> | null = null;

  provideCompletionItems(
    model: editor.ITextModel,
    position: Position,
    context: languages.CompletionContext,
    token: CancellationToken
  ): languages.ProviderResult<languages.CompletionList> {
    if (this.modules == null) return null;
    const cursorPosition = model.getOffsetAt(position);
    const module = this.modules.get("source.lf")!;
    const node = findClosestNode(module, cursorPosition);
    if (!node) return null;
    if (node.kind == "variable access") {
      if (node.type == UnknownType) return null;
    }
    throw new Error("Method not implemented.");
  }
}

function findClosestNode(module: Module, position: number): AstNode | null {
  let bestNode: AstNode | null = null;
  let bestDistance = Infinity;

  const callback = (node: AstNode, parentNode: AstNode | null): boolean => {
    if (node.location.start > position) return false;
    if (node.location.end > position) return false;
    const nodeLength = node.location.end - node.location.start;
    const distance = position - node.location.end;

    if (!bestNode) {
      bestNode = node;
      bestDistance = distance;
    } else {
      if (distance < bestDistance) {
        bestNode = node;
        bestDistance = distance;
      }
    }
    return true;
  };

  for (const statement of module.ast) {
    traverseAst(statement, null, callback);
  }
  return bestNode;
}

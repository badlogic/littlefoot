import "monaco-editor/min/vs/editor/editor.main.css";
import * as monaco from "monaco-editor";
import { LittleFootError, SourceLocation, keywords } from "../lib";
(globalThis as any).self.MonacoEnvironment = { getWorkerUrl: () => "./build/editor.worker.js" };

export class Editor {
  editor: monaco.editor.IStandaloneCodeEditor;

  constructor(
    container: HTMLElement,
    public changeCallback: (newText: string) => void = () => {},
    public selectionCallback: (start: number, end: number) => void = () => {}
  ) {
    defineLittleFootLanguage();
    this.editor = monaco.editor.create(container, {
      automaticLayout: true,
      quickSuggestions: false,
      language: "littlefoot",
      theme: "vs-dark",
      value: "",
      tabSize: 2,
      minimap: {
        enabled: false,
      },
      bracketPairColorization: {
        enabled: true,
      },
    });

    this.editor.onDidChangeModelContent(() => changeCallback(this.value));
    this.editor.onDidChangeCursorSelection((e) => {
      const start = this.editor.getModel()!.getOffsetAt(e.selection.getStartPosition());
      const end = this.editor.getModel()!.getOffsetAt(e.selection.getEndPosition());
      selectionCallback(start, end);
    });
  }

  set value(text: string) {
    this.editor.setValue(text);
  }

  get value() {
    return this.editor.getValue();
  }

  highlightErrors(errors: LittleFootError[]) {
    const markers: monaco.editor.IMarkerData[] = [];
    const markError = (location: SourceLocation, error: LittleFootError, markCauses: boolean) => {
      const startPos = this.editor.getModel()!.getPositionAt(location.start);
      const endPos = this.editor.getModel()!.getPositionAt(location.end);
      markers.push({
        severity: monaco.MarkerSeverity.Error,
        startLineNumber: startPos.lineNumber,
        startColumn: startPos.column,
        endLineNumber: endPos.lineNumber,
        endColumn: endPos.column,
        message: error.message,
      });
      if (error.supplementary.length > 0) {
        markers.push({
          severity: monaco.MarkerSeverity.Error,
          startLineNumber: startPos.lineNumber,
          startColumn: startPos.column,
          endLineNumber: endPos.lineNumber,
          endColumn: endPos.column,
          message: error.supplementary,
        });
      }
      if (error.cause && markCauses) {
        let cause: LittleFootError | null = error.cause;
        while (cause) {
          markError(location, cause, false);
          cause = cause.cause;
        }
      }
    };
    errors.forEach((error) => {
      markError(error.location, error, true);
      let cause = error.cause;
      while (cause) {
        markError(cause.location, cause, true);
        cause = cause.cause;
      }
    });
    monaco.editor.setModelMarkers(this.editor.getModel()!, "littlefoot", markers);
  }

  setCompletionProvider(provider: monaco.languages.CompletionItemProvider) {
    monaco.languages.registerCompletionItemProvider("littlefoot", provider);
  }
}

let languageDefined = false;
export function defineLittleFootLanguage() {
  if (languageDefined) return;
  monaco.languages.register({ id: "littlefoot" });
  monaco.languages.setLanguageConfiguration("littlefoot", {
    autoClosingPairs: [
      { open: "{", close: "}" },
      { open: "[", close: "]" },
      { open: "(", close: ")" },
    ],
    brackets: [
      ["(", ")"],
      ["{", "}"],
      ["[", "]"],
    ],
  });
  monaco.languages.setMonarchTokensProvider("littlefoot", {
    escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,
    keywords: [...keywords, "is", "as", "true", "false"],
    typeKeywords: ["nothing", "string", "boolean", "int8", "int16", "int32", "float64", "number"],
    brackets: [
      { open: "{", close: "}", token: "delimiter.curly" },
      { open: "[", close: "]", token: "delimiter.bracket" },
      { open: "(", close: ")", token: "delimiter.parenthesis" },
    ],
    tokenizer: {
      root: [
        [
          /[\p{Alphabetic}a-zA-Z_][\p{Alphabetic}a-zA-Z0-9_]*/u,
          {
            cases: {
              "@keywords": "keyword",
              "@typeKeywords": "type",
              "@default": "identifier",
            },
          },
        ],
        { include: "@whitespace" },
        [/[{}()\[\]]/, "@brackets"],
        [/\d*\.\d+([eE][\-+]?\d+)?/, "number.float"],
        [/0[x][0-9a-fA-F]+/, "number.hex"],
        [/0[b][0-1]+/, "number.hex"],
        [/\d+/, "number"],
        [/[;,.]/, "delimiter"],
        [/"([^"\\]|\\.)*$/, "string.invalid"], // non-teminated string
        [/"([^"\\]|\\[\s\S])*"/g, "string"], // Multi-line string
        [/"/, { token: "string.quote", bracket: "@open", next: "@string" }],
      ],
      string: [
        [/[^\\"]+/, "string"],
        [/@escapes/, "string.escape"],
        [/\\./, "string.escape.invalid"],
        [/"/, { token: "string.quote", bracket: "@close", next: "@pop" }],
      ],
      whitespace: [
        [/[ \t\r\n]+/, "white"],
        [/#.*$/, "comment"],
      ],
    },
  });
}

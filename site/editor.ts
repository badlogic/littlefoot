import "monaco-editor/min/vs/editor/editor.main.css";
import * as monaco from "monaco-editor";
import { LittleFootError, keywords } from "../lib";
(globalThis as any).self.MonacoEnvironment = { getWorkerUrl: () => "./build/editor.worker.js" };

export class Editor {
  editor: monaco.editor.IStandaloneCodeEditor;

  constructor(container: HTMLElement, public changeCallback: (newText: string) => void = () => {}) {
    defineLittleFootLanguage();
    this.editor = monaco.editor.create(container, {
      automaticLayout: true,
      language: "littlefoot",
      theme: "vs-dark",
      value: "",
      minimap: {
        enabled: false,
      },
      bracketPairColorization: {
        enabled: true,
      },
    });

    this.editor.onDidChangeModelContent(() => changeCallback(this.value));
  }

  set value(text: string) {
    this.editor.setValue(text);
  }

  get value() {
    return this.editor.getValue();
  }

  highlightErrors(errors: LittleFootError[]) {
    const markers: monaco.editor.IMarkerData[] = [];
    errors.forEach((error) => {
      const startPos = this.editor.getModel()!.getPositionAt(error.start);
      const endPos = this.editor.getModel()!.getPositionAt(error.end);
      markers.push({
        severity: monaco.MarkerSeverity.Error,
        startLineNumber: startPos.lineNumber,
        startColumn: startPos.column,
        endLineNumber: endPos.lineNumber,
        endColumn: endPos.column,
        message: error.message,
      });
    });
    monaco.editor.setModelMarkers(this.editor.getModel()!, "littlefoot", markers);
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
    keywords: [...keywords, "is"],
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
              "@default": "identifier",
            },
          },
        ],
        { include: "@whitespace" },
        [/[{}()\[\]]/, "@brackets"],
        [/\d*\.\d+([eE][\-+]?\d+)?/, "number.float"],
        [/0[x][0-9a-fA-F]+/, "number.hex"],
        [/\d+/, "number"],
        [/[;,.]/, "delimiter"],
        [/"([^"\\]|\\.)*$/, "string.invalid"], // non-teminated string
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

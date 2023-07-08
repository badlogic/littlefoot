import "monaco-editor/min/vs/editor/editor.main.css";
import * as monaco from "monaco-editor";
import { LittleFootError } from "../lib";
(globalThis as any).self.MonacoEnvironment = {
  getWorkerUrl: function (moduleId: any, label: any) {
    return "./build/editor.worker.js";
  },
};

export class Editor {
  editor: monaco.editor.IStandaloneCodeEditor;

  constructor(container: HTMLElement, public changeCallback: (newText: string) => void = () => {}) {
    this.editor = monaco.editor.create(container, {
      automaticLayout: true,
      language: "littlefoot",
      theme: "vs-dark",
      value: "",
      minimap: {
        enabled: false,
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

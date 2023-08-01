import { SourceLocation } from "./source";

export const INDENDATION = "   ";
export function indent(level: number = 1) {
  if (level < 1) return "";
  let indentation = "";
  for (let i = 0; i < level; i++) {
    indentation += INDENDATION;
  }
  return indentation;
}

export class LittleFootError {
  constructor(public readonly location: SourceLocation, public readonly message: string) {}

  toString() {
    const lines = this.location.source.indicesToLines(this.location.start, this.location.end);
    let highlight = `${this.location.toString()}: ${this.message}\n\n`;
    let index = lines[0].start;
    for (const line of lines) {
      const lineText = this.location.source.text.substring(line.start, line.end);
      highlight += lineText.replace("\n", "") + "\n";
      for (let i = 0; i < lineText.length; i++) {
        const char = lineText.charAt(i);
        if (index >= this.location.start && index < this.location.end && char != "\r" && char != "\n") {
          highlight += "^";
        } else {
          highlight += char == "\t" ? "\t" : " ";
        }
        index++;
      }
      highlight += "\n";
    }
    return highlight;
  }
}

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
  constructor(
    public readonly location: SourceLocation,
    public readonly message: string,
    public readonly supplementary: string = "",
    public readonly cause: LittleFootError | null = null
  ) {}

  toString() {
    const lines = this.location.source.indicesToLines(this.location.start, this.location.end);
    let highlight = `error >>> ${this.location.toString()}: ${this.message}\n\n`;
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
    if (this.supplementary.length > 0) {
      highlight += "\n" + this.supplementary + "\n";
    }
    return highlight;
  }

  toStringWithCauses() {
    let errorMessage = this.toString();
    let cause = this.cause;
    let level = 1;
    while (cause) {
      const causeMessage = cause
        .toString()
        .trim()
        .split("\n")
        .map((line) => indent(level) + line)
        .join("\n");
      errorMessage += "\n" + indent(level) + "Cause:\n" + causeMessage;
      level++;
      cause = cause.cause;
    }
    return errorMessage;
  }
}

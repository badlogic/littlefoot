import { Source, SourceLocation } from "./source";

export class LittleFootError {
  constructor(public readonly location: SourceLocation, public readonly message: string) {}

  toString() {
    const lines = this.location.source.indicesToLines(this.location.start, this.location.end);
    let highlight = `${this.location.source.path}:${lines[0].index}: ${this.message}\n\n`;
    let index = lines[0].start;
    for (const line of lines) {
      const lineText = this.location.source.text.substring(line.start, line.end);
      highlight += lineText.replace("\n", "") + "\n";
      for (let i = 0; i < lineText.length; i++) {
        const char = lineText.charAt(i);
        if (index >= this.location.start && index < this.location.end) {
          highlight += "^";
        } else {
          highlight += char == "\t" ? "\t" : " ";
        }
        index++;
      }
    }
    return highlight;
  }
}

export class LittleFootError {
  constructor(start, end, source, message) {
    this.start = start;
    this.end = end;
    this.source = source;
    this.message = message;
  }

  toString() {
    const lines = this.source.indicesToLines(this.start, this.end);
    let highlight = `${this.source.identifier}:${lines[0].index}: ${this.message}\n\n`;
    let index = lines[0].start;
    for (const line of lines) {
      const lineText = this.source.text.substring(line.start, line.end);
      highlight += lineText.replace("\n", "") + "\n";
      for (let i = 0; i < lineText.length; i++) {
        const char = lineText.charAt(i);
        if (index >= this.start && index < this.end) {
          highlight += "^";
        } else {
          highlight += char == "\t" ? "\t" : " ";
        }
        index++;
      }
    }
    return highlight;
  }

  static fromTokens(tokens, message) {
    return new LittleFootError(tokens[0].start, tokens[tokens.length - 1].end, tokens[0].source, message);
  }
}

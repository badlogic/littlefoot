export class Source {
  constructor(identifier, text) {
    this.identifier = identifier;
    this.text = text;
    this.lines = [];
    let line = {
      index: 1,
      start: 0,
      end: 0,
    };
    let i = 0;
    for (; i < text.length; i++) {
      const char = text.charAt(i);
      if (char == "\n") {
        line.end = i + 1;
        this.lines.push(line);
        if (i == text.length - 1) {
          line = null;
        } else {
          line = {
            index: line.index + 1,
            start: line.end,
            end: line.end + 1,
          };
        }
      }
    }
    if (line) {
      line.end = i + 1;
      this.lines.push(line);
    }
  }

  indicesToLines(start, end) {
    const startLine = this.lines.findIndex((line) => start >= line.start && start < line.end);
    const endLine = this.lines.findIndex((line) => end >= line.start && end < line.end);
    return this.lines.slice(startLine, endLine + 1);
  }
}

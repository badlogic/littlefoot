export class Line {
  constructor(public readonly index: number, public readonly start: number, public end: number) {}
}

export class Source {
  readonly lines: Line[];

  constructor(public identifier: string, public text: string) {
    this.lines = [];
    let line: Line = new Line(1, 0, 0);
    let i = 0;
    for (; i < text.length; i++) {
      const char = text.charAt(i);
      if (char == "\n") {
        line.end = i + 1;
        this.lines.push(line);
        if (i == text.length - 1) {
          return;
        } else {
          line = new Line(line.index + 1, line.end, line.end + 1);
        }
      }
    }
    line.end = i + 1;
    this.lines.push(line);
  }

  indicesToLines(start: number, end: number) {
    const startLine = this.lines.findIndex((line) => start >= line.start && start < line.end);
    const endLine = this.lines.findIndex((line) => end >= line.start && end <= line.end);
    return this.lines.slice(startLine, endLine + 1);
  }
}

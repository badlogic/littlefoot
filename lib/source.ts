import { LittleFootError } from "./error";
import { Token } from "./tokenizer";

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

export class SourceLocation {
  constructor(public readonly source: Source, public readonly start: number, public readonly end: number) {}

  get text() {
    return this.source.text.substring(this.start, this.end);
  }

  get lines() {
    return this.source.indicesToLines(this.start, this.end);
  }

  static from(loc1: SourceLocation, loc2: SourceLocation) {
    if (loc1.source != loc2.source)
      throw new LittleFootError(
        loc1,
        `Internal compiler error: constructing source location from two locations of different sources: ${loc1.source.identifier} != ${loc2.source.identifier}`
      );
    return new SourceLocation(loc1.source, loc1.start, loc2.end);
  }
}

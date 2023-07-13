import * as fs from "fs";
import * as path from "path";
import { Source, SourceLoader } from "../lib";

export class FileSourceLoader implements SourceLoader {
  constructor(public readonly baseDir: string) {}

  load(sourcePath: string): Source | null {
    const filePath = path.resolve(path.join(this.baseDir, sourcePath));
    const content = fs.readFileSync(filePath, "utf-8");
    return new Source(filePath, content);
  }
}

#!/usr/bin/env node

import esbuild from "esbuild";

let watch = process.argv.length >= 3 && process.argv[2] == "--watch";

const buildContext = await esbuild.context({
  entryPoints: {
    index: "site/index.ts",
    "editor.worker": "monaco-editor/esm/vs/editor/editor.worker.js",
  },
  bundle: true,
  sourcemap: true,
  outdir: "site/build/",
  loader: {
    ".ttf": "dataurl",
    ".lf": "text",
    ".html": "text",
  },
  logLevel: "info",
  minify: !watch,
});
if (!watch) {
  await buildContext.rebuild();
} else {
  buildContext.watch();
}

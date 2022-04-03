#!/usr/bin/env node

import esbuildServe from "esbuild-serve";

esbuildServe(
  {
    logLevel: "info",
    // entryPoints: ["src/textAnimations.js"],
    entryPoints: ["src/layout.js"],
    bundle: true,
    outfile: "public/index.js",
  },
  { root: "public" }
);

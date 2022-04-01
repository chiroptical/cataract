#!/usr/bin/env node

import esbuildServe from "esbuild-serve";

esbuildServe(
  {
    logLevel: "info",
    entryPoints: ["src/basic.js"],
    bundle: true,
    outfile: "dist/basic.js",
  },
  { root: "dist" }
);

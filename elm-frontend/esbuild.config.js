#!/usr/bin/env node

import ElmPlugin from "esbuild-plugin-elm";
import esbuildServe from "esbuild-serve";

esbuildServe(
  {
    entryPoints: ["src/index.js"],
    bundle: true,
    outfile: "public/index.js",
    plugins: [
      ElmPlugin({
        debug: true,
        clearOnWatch: false,
      }),
    ],
  },
  { root: "public" }
)

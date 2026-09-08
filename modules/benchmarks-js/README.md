# AGS Scala.js benchmark

Times `AgsParams.posCalculations` (the per-posAngle overlay build) and the full
`Ags.agsAnalysis` on the workload from `docs/ags-parallelization.md`: GMOS imaging,
36 position angles, 20/30/50 science offsets, 91 candidates.

Node:

    sbt "benchmarksJS/run 20,30,50"

Browser (ES module, needs an http server, not file://):

    sbt benchmarksJS/fastLinkJS
    cd modules/benchmarks-js && python3 -m http.server 8000
    open http://localhost:8000/?offsets=20,30

Output lands in `target/scala-3.9.0/lucuma-benchmarks-js-fastopt/main.js`. Use `fullLinkJS`
(and edit the script path in `index.html`) for optimized numbers.
Only same-session, paired runs are comparable; see the methodology note in
`docs/relateng-benchmark-findings.md`.

The module deliberately avoids cats-effect so it can link under the Scala.js
WebAssembly backend (`withESFeatures(_.withUseWebAssembly(true))`).

Headless Chrome, no extension needed (dumps the `<pre>` after the run):

    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" --headless=new \
      --virtual-time-budget=120000 --dump-dom 'http://localhost:8000/?offsets=20,30,50' \
      | sed -n '/<pre/,/<\/pre>/p'

Scripted Chrome runs, for pages whose main starts asynchronously (the wasm build):

    node chrome-run.mjs 'http://localhost:8000/index-opt.html?offsets=20,30,50' 100

The second argument is seconds to wait before reading the output. The script deliberately
does not enable the DevTools `Runtime` domain while the benchmark runs: an attached
debugger disables wasm tier-up and made the wasm build run 5x slower than reality.
`--dump-dom --virtual-time-budget` is also unusable for the wasm build (clock frozen).

## WebAssembly variant

`benchmarksWasm` links the same sources with the Scala.js WebAssembly backend
(`modules/benchmarks-wasm`, output `main.mjs` + `main.wasm`). Needs Node >= 25
(Node 24 fails with `invalid value type 'exn'`), Chrome 137+, Firefox 134+, Safari 26+.

## Native kernel variant (Rust `geo` -> wasm)

`agsgeo/` is a prototype Rust crate (geo 0.33 + wasm-bindgen) exposing a handle arena of
polygons: constructors, boolean ops, affine transform, area, bbox, contains, intersects.
`GeoWasm.scala` is the Scala.js facade plus a `ShapeInterpreter` mirroring the JTS one.
`KernelBench.scala` evaluates the posCalculations geometry with both interpreters in the
same process and checks parity. Pass `kernel` (Node) or `&kernel` (browser) to run only that.

Build the kernel (needs a rustup toolchain with the wasm32 target; the nix rustc has none):

    export PATH=$HOME/.rustup/toolchains/stable-aarch64-apple-darwin/bin:$HOME/.cargo/bin:$PATH
    export DYLD_FALLBACK_LIBRARY_PATH=$HOME/.rustup/toolchains/stable-aarch64-apple-darwin/lib
    cd agsgeo && wasm-pack build --release --target web

The `DYLD_FALLBACK_LIBRARY_PATH` line works around rust-lld not finding libLLVM.dylib in a
rustup minimal-profile install on macOS.

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

Output lands in `target/scala-3.9.0/lucuma-benchmarks-js-fastopt/main.js`; `index-opt.html`
loads the `fullLinkJS` output. Compare optimized builds only, and only same-session, paired
runs; see the methodology notes in `docs/ags-wasm-findings.md` and
`docs/relateng-benchmark-findings.md`.

Headless Chrome, no extension needed (dumps the `<pre>` after the run):

    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" --headless=new \
      --virtual-time-budget=120000 --dump-dom 'http://localhost:8000/?offsets=20,30,50' \
      | sed -n '/<pre/,/<\/pre>/p'

Scripted Chrome runs, for pages whose main starts asynchronously (the `wasm` mode):

    node chrome-run.mjs 'http://localhost:8000/index-opt.html?offsets=20,30,50&wasm' 100

The second argument is seconds to wait before reading the output. The script deliberately
does not enable the DevTools `Runtime` domain while the benchmark runs: an attached debugger
disables wasm tier-up and made wasm run 5x slower than reality. `--dump-dom
--virtual-time-budget` is unusable for async mains (clock frozen).

## Kernel mode, end to end

Pass `wasm` (Node; in a browser `&wasm` also needs an import map or bundler for the bare
`lucuma-geo-wasm` specifier) to load the `lucuma-geo-wasm` npm package through
`WasmGeometry.loadFrom` and run the full `Ags.agsAnalysis` on JTS and on the kernel, paired per
rep, with result histograms and the kernel handle count left behind by each run:

    npm ci
    sbt benchmarksJS/fullLinkJS
    node modules/benchmarks-js/target/scala-3.9.0/lucuma-benchmarks-js-opt/main.js 20,30,50 wasm

Node resolves the package from the repo-root `node_modules`. Use Node 26 or newer for the wasm
runs. The kernel crate itself lives in https://github.com/cquiroz/lucuma-geo-wasm.

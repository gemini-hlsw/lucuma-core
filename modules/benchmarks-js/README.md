# AGS Scala.js benchmark

Times `AgsParams.posCalculations` (the per-posAngle overlay build) and the full
`Ags.agsAnalysis` on a request captured in Explore (GMOS imaging, PWFS1, 36 position angles,
50 science offsets, 121 Gaia candidates; `modules/benchmarks-shared`). The first argument lists
workloads: `real` replays the request as is, a number N swaps in an N-point offset grid.

Node:

    sbt "benchmarksJS/run real,20,50"

Compare `fullLinkJS` builds only, and only same-session, paired runs. Take browser numbers
from Explore itself, with DevTools closed while timing: an attached debugger disables wasm
tier-up and made the kernel look 5x slower than it is.

## Kernel mode, end to end

Pass `wasm` to load the `@gemini-hlsw/lucuma-wasm` npm package through
`WasmGeometry.loadFrom` and run the full `Ags.agsAnalysis` on JTS and on the kernel, paired per
rep, with result histograms and the kernel handle count left behind by each run:

    npm ci
    sbt benchmarksJS/fullLinkJS
    node modules/benchmarks-js/target/scala-3.9.0/lucuma-benchmarks-js-opt/main.js real,50,100 wasm

Node resolves the package from the repo-root `node_modules`. Use Node 26 or newer for the wasm
runs. The kernel crate itself lives in https://github.com/gemini-hlsw/lucuma-wasm.

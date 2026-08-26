# lucuma-benchmarks

JMH micro-benchmarks for the AGS.
The benchmark data (real GMOS / Flamingos2 OIWFS geometries) lives in
`Fixtures.scala`; the timed scenarios are in `RelateNgBenchmarks.scala` and
`S4IntersectionBenchmark.scala`.

## Running the benchmarks

The suite uses [sbt-jmh](https://github.com/sbt/sbt-jmh). Run from the repo root:

```bash
# all benchmarks
sbt "benchmarks/Jmh/run -i 5 -wi 5 -f1"

# a subset by regex (S1/S2/S3/S5 — average time, ns/op)
sbt "benchmarks/Jmh/run -i 5 -wi 5 -f1 .*RelateNgBenchmarks.*"

# the S4 offset sweep only (SampleTime, ms; @Param over 10/50/100 offsets)
sbt "benchmarks/Jmh/run .*S4IntersectionBenchmark.*"
```

Flags: `-i` measurement iterations, `-wi` warmup iterations, `-f` forks, `-r` /
`-w` per-iteration time. JMH forks each trial into a fresh JVM, so a single
invocation already isolates JIT profiles between scenarios. Full options:
`sbt "benchmarks/Jmh/run -h"`.

### Scenarios

| Benchmark | Scenario | Mode |
|---|---|---|
| `RelateNgBenchmarks.s1_gmosContains` / `s1_f2Contains` | S1 `polygon.contains(point)` — candidate-star containment | avg ns/op |
| `RelateNgBenchmarks.s2_intersects` | S2 `probeArm.intersects(science)` — vignetting | avg ns/op |
| `RelateNgBenchmarks.s3_intersectionArea` | S3 `intersection().getArea` — overlay | avg ns/op |
| `RelateNgBenchmarks.s5_indexedLocator` | S5 `IndexedPointInAreaLocator.locate` — prepared ceiling | avg ns/op |
| `S4IntersectionBenchmark.buildAndEval` | S4 N-offset patrol-field overlay build+eval — the AGS slow path | SampleTime ms |

## Comparing two versions (A/B)
### 1. Run each version


```bash
# legacy: ensure build.sbt has  lazy val jtsVersion = "0.4.2"
sbt 'benchmarks/Jmh/run -f 5 -wi 8 -w 2 -i 10 -r 2 -t 1 -gc true -rf json -rff modules/benchmarks/legacy.json .*RelateNgBenchmarks.*'

# RelateNG: flip the comments so the snapshot is uncommented, then
sbt 'benchmarks/Jmh/run -f 5 -wi 8 -w 2 -i 10 -r 2 -t 1 -gc true -rf json -rff modules/benchmarks/relateng.json .*RelateNgBenchmarks.*'
```

For the S4 overlay sweep (ms-scale, `SampleTime`), fewer/shorter knobs suffice:

```bash
sbt 'benchmarks/Jmh/run -f 3 -wi 5 -i 10 -gc true -rf json -rff modules/benchmarks/legacy-s4.json .*S4IntersectionBenchmark.*'
# ...flip the version...
sbt 'benchmarks/Jmh/run -f 3 -wi 5 -i 10 -gc true -rf json -rff modules/benchmarks/relateng-s4.json .*S4IntersectionBenchmark.*'
```

### 2. Diff the JSON

JMH has no built-in differential. From `modules/benchmarks/`, the predicates:

```bash
jq -nr --slurpfile a legacy.json --slurpfile b relateng.json '
  ($a[0] | map({(.benchmark|sub(".*\\.";"")): .primaryMetric.score}) | add) as $before |
  ($b[0] | map({(.benchmark|sub(".*\\.";"")): .primaryMetric.score}) | add) as $after |
  "scenario\tbefore\tafter\tspeedup",
  ($before|keys[] as $k | "\($k)\t\($before[$k]|.*100|round/100)\t\($after[$k]|.*100|round/100)\t\(($before[$k]/$after[$k])|.*100|round/100)x")
' | column -t
```

For S4, pair by the offset `@Param` instead of the benchmark name:

```bash
jq -nr --slurpfile a legacy-s4.json --slurpfile b relateng-s4.json '
  ($a[0] | map({(.params.offsets): .primaryMetric.score}) | add) as $before |
  ($b[0] | map({(.params.offsets): .primaryMetric.score}) | add) as $after |
  "offsets\tbefore(ms)\tafter(ms)\tspeedup",
  ($before|keys[] as $k | "\($k)\t\($before[$k]|.*100|round/100)\t\($after[$k]|.*100|round/100)\t\(($before[$k]/$after[$k])|.*100|round/100)x")
' | column -t
```

## Sample results (legacy 0.4.2 vs RelateNG snapshot)

From a paired A/B (`-i 5 -wi 5 -f1`) on the predicates. Lower ns/op is faster.

| Scenario | Legacy 0.4.2 (ns/op) | RelateNG (ns/op) | Speedup |
|---|---:|---:|---:|
| `s1_f2Contains` | 2506.14 | 145.83 | 17.2× |
| `s1_gmosContains` | 1820.07 | 167.78 | 10.8× |
| `s2_intersects` | 259.01 | 48.78 | 5.3× |
| `s3_intersectionArea` | 983.44 | 1056.45 | 0.93×  |
| `s5_indexedLocator` | 25.33 | 25.65 | 1.0×  |


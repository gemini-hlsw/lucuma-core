// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.bench

/**
 * Minimal micro-benchmark in lieu of JMH.
 * the JIT cannot dead-code-eliminate the work; we accumulate it into a sink.
 */
object Bench:

  case class Result(name: String, opsPerSec: Double, nsPerOp: Double, iterations: Long):
    override def toString: String =
      f"$name%-44s ${opsPerSec}%14.1f ops/s  ${nsPerOp}%12.1f ns/op  (${iterations}%,d iters)"

  // Black hole: prevents the JIT from eliding the measured work.
  // Taken from JMH
  @volatile private var sink: Long = 0L

  private def consume(v: Long): Unit =
    sink = sink ^ v

  def run(name: String, warmup: Int = 50_000, iterations: Int = 200_000)(body: => Long): Result =
    var i = 0
    while i < warmup do
      consume(body)
      i += 1
    val start = System.nanoTime()
    i = 0
    var acc = 0L
    while i < iterations do
      acc += body
      i += 1
    val elapsed = System.nanoTime() - start
    consume(acc)
    val nsPerOp   = elapsed.toDouble / iterations
    val opsPerSec = 1e9 / nsPerOp
    Result(name, opsPerSec, nsPerOp, iterations.toLong)

  def section(title: String): Unit =
    println()
    println(title)
    println("-" * title.length)

  def blackHole: Long = sink

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom

/**
 * Interprets a `ShapeExpression` to a `Shape`.
 */
trait ShapeInterpreter {
  def interpret(e: ShapeExpression): Shape

  /**
   * Runs `f` in an arena: every `Shape` created inside may be released when it returns. Engines
   * with automatic memory management do nothing; native engines free their handles on exit, so
   * shapes created inside must not escape the block.
   */
  def withArena[A](f: => A): A = f
}

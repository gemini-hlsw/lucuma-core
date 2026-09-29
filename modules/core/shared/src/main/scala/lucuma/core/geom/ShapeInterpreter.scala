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

object ShapeInterpreter {

  /**
   * Engine used wherever no interpreter is given explicitly. JTS until a native kernel is loaded
   * and installs itself (see `lucuma.core.geom.wasm.WasmGeometry` on Scala.js).
   */
  @volatile var default: ShapeInterpreter = jts.JtsShapeInterpreter

  /** Delegates to `default` at call time; an explicit lexical `given` still takes precedence. */
  given ShapeInterpreter with {
    def interpret(e: ShapeExpression): Shape = default.interpret(e)
    override def withArena[A](f: => A): A    = default.withArena(f)
  }

}

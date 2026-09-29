// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom

/**
 * `import lucuma.core.geom.auto.given` for the engine chosen at run time: `ShapeInterpreter.default`,
 * read on every call, so a kernel loaded later takes over.
 */
object auto:
  given ShapeInterpreter with
    def interpret(e: ShapeExpression): Shape = ShapeInterpreter.default.interpret(e)
    override def withArena[A](f: => A): A    = ShapeInterpreter.default.withArena(f)

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.jts
package demo

import lucuma.core.geom.gmos._
import lucuma.core.geom.jts.JtsShape
import lucuma.core.geom.jts.interpreter._
import lucuma.core.geom.svg._
import lucuma.core.geom.svg.implicits._
import lucuma.svgdotjs.Svg

/**
 * Throwaway demo code to visualize a shape created using `ShapeExpression`s.
 */
object JtsDemo {
  def main(args: Array[String]): Unit =
    probeArm.shape.eval match {
      case j: JtsShape =>
        val svg: Svg = new Svg()
        j.toSvg(svg)
        println(svg)
      case _           => throw new RuntimeException()
    }
}

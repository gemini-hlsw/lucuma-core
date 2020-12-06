// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.jts
package demo

import lucuma.svgdotjs.Svg
import lucuma.core.geom.GmosOiwfsProbeArm
import lucuma.core.geom.jts.JtsShape
import lucuma.core.geom.jts.interpreter._
import lucuma.core.geom.svg._
import lucuma.core.geom.svg.implicits._

/**
 * Throwaway demo code to visualize a shape created using `ShapeExpression`s.
 */
object JtsDemo {
  def main(args: Array[String]): Unit =
    GmosOiwfsProbeArm.shape.eval match {
      case j: JtsShape =>
        val svg: Svg = new Svg()
        j.toSvg(svg)
        println(svg)
      case _           => throw new RuntimeException()
    }
}

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.math.geom.jts
package demo

import gpp.svgdotjs.svgdotjsSvgJs.mod.SVG_
import gpp.svgdotjs.svgdotjsSvgJs.mod.Svg
import lucuma.math.geom.jts.JtsShape
import lucuma.math.geom.jts.interpreter._
import lucuma.math.geom.svg._
import lucuma.math.geom.svg.implicits._

/**
  * Throwaway demo code to visualize a shape created using `ShapeExpression`s.
  */
object JtsDemo {
  def main(args: Array[String]): Unit = {
    println("here")
    println(lucuma.math.geom.svg.implicits.renderJtsShapeMapOfIds)
    GmosOiwfsProbeArm.shape.eval match {
      case j: JtsShape =>
        val svg: Svg = SVG_()
        j.toSvg(svg)
        println(svg)
      case _ => throw new RuntimeException()
    }
  }
}

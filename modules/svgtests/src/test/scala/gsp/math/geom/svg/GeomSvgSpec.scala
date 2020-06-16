// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.geom.svg

import gsp.math.geom.jts.interpreter._
import gsp.math.geom.svg._
import gsp.math.geom.svg.implicits._
import gsp.math.geom.ShapeExpression
import gsp.math.geom.syntax.all._
import gsp.math.geom.jts.demo.GmosScienceAreaGeometry
import gsp.math.geom.jts.demo.GmosOiwfsProbeArm
import gsp.math.geom.jts.JtsShape
import gsp.math.syntax.int._
import gsp.math.Angle
import gsp.math.Offset
import gpp.svgdotjs.svgdotjsSvgJs.mod.SVG_
import gpp.svgdotjs.svgdotjsSvgJs.mod.Svg

final class GeomSvgSpec extends munit.FunSuite {
  val posAngle: Angle =
    145.deg

  val guideStarOffset: Offset =
    Offset(170543999.µas.p, -24177003.µas.q)

  val offsetPos: Offset =
    Offset(60.arcsec.p, 60.arcsec.q)

  // TODO: should come from the FPUnit enum in core
  val ifuOffset: Offset =
    Offset.Zero

  // TODO: when we move this core there should be an enum
  val sideLooking: Boolean =
    true

  // Shapes and amount of children expected
  val shapes: List[ShapeExpression] =
    List(
      GmosOiwfsProbeArm.shape ⟲ 45.deg,
      GmosOiwfsProbeArm.shapeAt(posAngle, guideStarOffset, offsetPos, ifuOffset, sideLooking),
      GmosOiwfsProbeArm.patrolFieldAt(posAngle, offsetPos, ifuOffset, sideLooking),
      GmosScienceAreaGeometry.imaging ↗ offsetPos ⟲ posAngle
    )

  test("basic") {
    shapes.foreach {
      case shape =>
        shape.eval match {
          case jts: JtsShape =>
            val svg: Svg = SVG_()
            jts.toSvg(svg)
            assert(svg.children().length.toInt > 0)
          case x             => sys.error(s"Whoa unexpected shape type: $x")
        }
    }
  }
}

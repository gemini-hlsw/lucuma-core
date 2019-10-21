// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.geom

package jts.demo

import gsp.math.Angle

import gsp.math.syntax.int._
import gsp.math.geom.syntax.all._

// WIP: would need to be moved out of this package and is missing the
// calculation for particular offset position impact on the arm rotation

/**
 * Description of the GMOS OIWFS probe arm geometry.
 */
object GmosOiwfsProbeArm {
  val PickoffArmLength: Angle      = 358460.mas
  val PickoffMirrorSize: Angle     =     20.arcsec
  val ProbeArmLength: Angle        = PickoffArmLength - PickoffMirrorSize.bisect
  val ProbeArmTaperedWidth: Angle  =     15.arcsec
  val ProbeArmTaperedLength: Angle =    180.arcsec

  private val arm: ShapeExpression = {
    val hm: Angle  = PickoffMirrorSize.bisect
    val htw: Angle = ProbeArmTaperedWidth.bisect

    val p0 = (-hm,                           htw  )
    val p1 = (p0._1 - ProbeArmTaperedLength, hm   )
    val p2 = (p0._1 - ProbeArmLength,        p1._2)
    val p3 = (p2._1,                        -hm   )
    val p4 = (p1._1,                         p3._2)
    val p5 = (p0._1,                        -htw  )

    ShapeExpression.polygonAt(p0, p1, p2, p3, p4, p5, p0)
  }

  private val pickoff: ShapeExpression = {
    val s = PickoffMirrorSize.bisect
    ShapeExpression.rectangleAt((s, s), (s - PickoffMirrorSize, s - PickoffMirrorSize))
  }

  /**
   * Description of the GMOS OIWFS probe arm with the pickoff mirror centered
   * at the base position.
   */
  val shape: ShapeExpression =
    arm ∪ pickoff

}

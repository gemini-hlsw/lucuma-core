// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.geom

package jts.demo
import gsp.math.{ Angle, Offset }
import gsp.math.syntax.int._
import gsp.math.geom.syntax.all._

import cats.implicits._
import scala.math.{ Pi, asin, atan2, hypot, sin }

// TODO: move to core

/**
 * Description of the GMOS OIWFS probe arm geometry.
 */
object GmosOiwfsProbeArm {
  private val PickoffArmLength: Angle      = 358460.mas
  private val PickoffMirrorSize: Angle     =     20.arcsec
  private val ProbeArmLength: Angle        = PickoffArmLength - PickoffMirrorSize.bisect
  private val ProbeArmTaperedWidth: Angle  =     15.arcsec
  private val ProbeArmTaperedLength: Angle =    180.arcsec

  private val StageArmLength: Angle        = 124890.mas

  private val arm: ShapeExpression = {
    val hm: Angle  = PickoffMirrorSize.bisect
    val htw: Angle = ProbeArmTaperedWidth.bisect

    val p0 = (-hm.p,                                                   htw.q)
    val p1 = (Offset.P.angle.modify(_ - ProbeArmTaperedLength)(p0._1), hm.q )
    val p2 = (Offset.P.angle.modify(_ - ProbeArmLength)(p0._1),        p1._2)
    val p3 = (p2._1,                                                  -hm.q )
    val p4 = (p1._1,                                                   p3._2)
    val p5 = (p0._1,                                                  -htw.q)

    ShapeExpression.polygonAt(p0, p1, p2, p3, p4, p5, p0)
  }

  private val pickoff: ShapeExpression = {
    val s = PickoffMirrorSize.bisect
    ShapeExpression.rectangleAt((s.p, s.q), ((s - PickoffMirrorSize).p, (s - PickoffMirrorSize).q))
  }

  /**
   * Description of the GMOS OIWFS probe arm with the pickoff mirror centered
   * at the base position.
   */
  val shape: ShapeExpression =
    arm ∪ pickoff

  /**
   * The GMOS OIWFS probe arm positioned to reach a particular guide star at
   * a particular offset.
   *
   * @param posAngle position angle where positive is counterclockwise
   * @param guideStar guide star offset from the center, relative to an un-rotated frame
   * @param offsetPos offset position from the base, if any
   * @param ifuOffset correction for IFU FP-units, if any // TODO: replace with actual FPUnit
   * @param sideLooking `true` when mounted on a side-looking port, `false` for up-looking // TODO: replace with enum
   *
   * @return probe arm shape correctly rotated and offset to reach the guide star
   */
  def shapeAt(
    posAngle:    Angle,
    guideStar:   Offset,
    offsetPos:   Offset,
    ifuOffset:   Offset,
    sideLooking: Boolean
  ): ShapeExpression =
    shape ⟲ armAngle(posAngle, guideStar, offsetPos, ifuOffset, sideLooking) ↗ guideStar

  private def armAngle(
    posAngle:    Angle,
    guideStar:   Offset,
    offsetPos:   Offset,
    ifuOffset:   Offset,
    sideLooking: Boolean
  ): Angle = {

    val t   = Offset(427520.mas.p, 101840.mas.q)
    val tʹ  = if (sideLooking) Offset.qAngle.modify(_.mirrorBy(Angle.Angle0))(t) else t

    val bx  = Angle.signedDecimalArcseconds.get(StageArmLength).toDouble
    val bxᒾ = bx*bx

    val mx  = Angle.signedDecimalArcseconds.get(PickoffArmLength).toDouble
    val mxᒾ = mx*mx

    val (x, y) =
      Offset.signedDecimalArcseconds.get(
        tʹ.rotate(posAngle) + guideStar - (offsetPos - ifuOffset).rotate(posAngle)
      ).bimap(x => -x.toDouble, _.toDouble)

    val r  = hypot(x, y)
    val rᒾ = r * r
    val α  = atan2(x, -y)

    val φ  =
      math.acos(
        (rᒾ - (bxᒾ + mxᒾ)) / (2.0 * bx * mx) match {
          case a if a < -1.0 => -1.0
          case a if a >  1.0 =>  1.0
          case a             =>    a
        }
      ) * (if (sideLooking) -1.0 else 1.0)

    val θ  = {
      val θʹ = asin((mx / r) * sin(φ))
      if (mxᒾ > (rᒾ + bxᒾ)) Pi - θʹ else θʹ
    }

    Angle.fromDoubleRadians(-φ + θ + α + Pi / 2.0)
  }

  /**
   * GMOS patrol field shape centered at the base position.
   */
  val patrolField: ShapeExpression =
    ShapeExpression.centeredRectangle(212700.mas, 249600.mas)

  /**
   * GMOS patrol field shape, in context.
   *
   * @param posAngle position angle where positive is counterclockwise
   * @param offsetPos offset position from the base, if any
   * @param ifuOffset correction for IFU FP-units, if any // TODO: replace with actual FPUnit
   * @param sideLooking `true` when mounted on a side-looking port, `false` for up-looking // TODO: replace with enum
   *
   * @return probe field shape rotated and offset
   */
  def patrolFieldAt(
    posAngle:    Angle,
    offsetPos:   Offset,
    ifuOffset:   Offset,
    sideLooking: Boolean
  ): ShapeExpression = {
    val pf = patrolField ↗ (ifuOffset - Offset(94950.mas.p, 89880.mas.q))
    val s  = if (sideLooking) pf.flipQ else pf
    s ↗ offsetPos  ⟲ posAngle
  }

}

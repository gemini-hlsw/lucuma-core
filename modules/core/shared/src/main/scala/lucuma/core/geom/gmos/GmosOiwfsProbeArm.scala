// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.gmos

import cats.syntax.all._
import lucuma.core.enum.GmosNorthFpu
import lucuma.core.enum.GmosSouthFpu
import lucuma.core.enum.PortDisposition
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all._
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int._

import scala.math.Pi
import scala.math.asin
import scala.math.atan2
import scala.math.hypot
import scala.math.sin

/**
  * Description of the GMOS OIWFS probe arm geometry.
  */
trait GmosOiwfsProbeArm {
  private val PickoffArmLength: Angle      = 358460.mas
  private val PickoffMirrorSize: Angle     = 20.arcsec
  private val ProbeArmLength: Angle        = PickoffArmLength - PickoffMirrorSize.bisect
  private val ProbeArmTaperedWidth: Angle  = 15.arcsec
  private val ProbeArmTaperedLength: Angle = 180.arcsec

  private val StageArmLength: Angle = 124890.mas

  private val arm: ShapeExpression = {
    val hm: Angle  = PickoffMirrorSize.bisect
    val htw: Angle = ProbeArmTaperedWidth.bisect

    val p0 = (-hm.p, htw.q)
    val p1 = (Offset.P.angle.modify(_ - ProbeArmTaperedLength)(p0._1), hm.q)
    val p2 = (Offset.P.angle.modify(_ - ProbeArmLength)(p0._1), p1._2)
    val p3 = (p2._1, -hm.q)
    val p4 = (p1._1, p3._2)
    val p5 = (p0._1, -htw.q)

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

  private def ifuOffset(fpu: Option[Either[GmosNorthFpu, GmosSouthFpu]]): Offset =
    fpu.fold(Angle.Angle0)(_.fold(_.xOffset, _.xOffset)).offsetInP

  /**
    * The GMOS OIWFS probe arm positioned to reach a particular guide star at
    * a particular offset.
    *
    * @param posAngle position angle where positive is counterclockwise
    * @param guideStar guide star offset from the center, relative to an un-rotated frame
    * @param offsetPos offset position from the base, if any
    * @param fpu focal plane unit, if any
    * @param port port disposition
    *
    * @return probe arm shape correctly rotated and offset to reach the guide star
    */
  def shapeAt(
    posAngle:  Angle,
    guideStar: Offset,
    offsetPos: Offset,
    fpu:       Option[Either[GmosNorthFpu, GmosSouthFpu]],
    port:      PortDisposition
  ): ShapeExpression =
    shape ⟲ armAngle(posAngle, guideStar, offsetPos, fpu, port) ↗ guideStar

  private def armAngle(
    posAngle:  Angle,
    guideStar: Offset,
    offsetPos: Offset,
    fpu:       Option[Either[GmosNorthFpu, GmosSouthFpu]],
    port:      PortDisposition
  ): Angle = {

    val t  = Offset(427520.mas.p, 101840.mas.q)
    val tʹ =
      if (port === PortDisposition.Side) Offset.qAngle.modify(_.mirrorBy(Angle.Angle0))(t) else t

    val bx  = Angle.signedDecimalArcseconds.get(StageArmLength).toDouble
    val bxᒾ = bx * bx

    val mx  = Angle.signedDecimalArcseconds.get(PickoffArmLength).toDouble
    val mxᒾ = mx * mx

    val (x, y) =
      Offset.signedDecimalArcseconds
        .get(
          tʹ.rotate(posAngle) + guideStar - (offsetPos - ifuOffset(fpu)).rotate(posAngle)
        )
        .bimap(x => -x.toDouble, _.toDouble)

    val r  = hypot(x, y)
    val rᒾ = r * r
    val α  = atan2(x, -y)

    val φ =
      math.acos(
        (rᒾ - (bxᒾ + mxᒾ)) / (2.0 * bx * mx) match {
          case a if a < -1.0 => -1.0
          case a if a > 1.0  => 1.0
          case a             => a
        }
      ) * (if (port === PortDisposition.Side) -1.0 else 1.0)

    val θ = {
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
    * @param fpu focal plane unit, if any
    * @param port port disposition
    *
    * @return probe field shape rotated and offset
    */
  def patrolFieldAt(
    posAngle:  Angle,
    offsetPos: Offset,
    fpu:       Option[Either[GmosNorthFpu, GmosSouthFpu]],
    port:      PortDisposition
  ): ShapeExpression = {
    val pf = patrolField ↗ (ifuOffset(fpu) - Offset(94950.mas.p, 89880.mas.q))
    val s  = if (port === PortDisposition.Side) pf.flipQ else pf
    s ↗ offsetPos ⟲ posAngle
  }

}

object probeArm extends GmosOiwfsProbeArm

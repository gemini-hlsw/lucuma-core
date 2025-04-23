// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.f2

import cats.syntax.all.*
import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.units.accepted.*
import coulomb.units.si.prefixes.*
import lucuma.core.enums.F2LyotWheel
import lucuma.core.enums.PortDisposition
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.units.*

/**
  * Description of the F2 OIWFS probe arm geometry.
  */
trait F2OiwfsProbeArm:

  private def arm(plateScale: PlateScale): ShapeExpression = {
    val scaledLength = (ProbePickoffArmLength ⨱ plateScale).toAngle
    val hm = (PickoffMirrorSize ⨱ plateScale).toAngle.bisect
    val htw = ProbeArmTaperedWidth.bisect

    val (x0, y0) = (hm, -htw)
    val (x1, y1) = (x0 + ProbeArmTaperedLength, -hm)
    val (x2, y2) = (x0 + scaledLength, y1)
    val (x3, y3) = (x2, hm)
    val (x4, y4) = (x1, y3)
    val (x5, y5) = (x0, htw)

    ShapeExpression.polygonAt((x0.p, y0.q), (x1.p, y1.q), (x2.p, y2.q), (x3.p, y3.q), (x4.p, y4.q), (x5.p, y5.q))
  }

  private def pickoff(plateScale: PlateScale): ShapeExpression =
    val scaledMirrorSize = (PickoffMirrorSize ⨱ plateScale).toAngle
    ShapeExpression.centeredRectangle(scaledMirrorSize, scaledMirrorSize)

  /**
    * Description of the F2 OIWFS probe arm with the pickoff mirror centered
    * at the base position.
    */
  def shape(plateScale: PlateScale): ShapeExpression =
    arm(plateScale) ∪ pickoff(plateScale)

  /**
    * The F2 OIWFS probe arm positioned to reach a particular guide star at
    * a particular offset.
    *
    * @param posAngle position angle where positive is counterclockwise
    * @param guideStar guide star offset from the center, relative to an un-rotated frame
    * @param offsetPos offset position from the base, if any
    * @param lyot Lyot Wheel position, to get the plate scale
    * @param port port disposition
    *
    * @return probe arm shape correctly rotated and offset to reach the guide star
    */
  def shapeAt(
    posAngle:  Angle,
    guideStar: Offset,
    offsetPos: Offset,
    lyot:      F2LyotWheel,
    port:      PortDisposition
  ): ShapeExpression =
    val plateScale = lyot.plateScale
    shape(plateScale) ⟲ armAngle(posAngle, guideStar, offsetPos, port, plateScale) ↗ guideStar

  /**
    * Calculate the angle of the probe arm to reach a guide star at a given offset.
    */
  private def armAngle(posAngle:    Angle,
                       gsOffset:    Offset,
                       offset:      Offset,
                       port:        PortDisposition,
                       plateScale:  PlateScale): Angle = {
    // All calculations are done in arcseconds and just bigdecimal and doubles
    val Q = {
      val P = {
        val scaledFlippedPAO = {
          val scaledPAO = (ProbeArmOffset ⨱ plateScale).value
          if (port === PortDisposition.Bottom) -scaledPAO else scaledPAO
        }
        val angle = -posAngle
        (scaledFlippedPAO * angle.cos, scaledFlippedPAO * angle.sin)
      }

      val guideStar = (gsOffset - offset.rotate(posAngle)).inverse().toDoubleArcseconds
      val D = ((guideStar._1 - P._1), (guideStar._2 - P._2))

      val scaledPBAL = (ProbeBaseArmLength ⨱ plateScale).value
      val scaledPPAL = (ProbePickoffArmLength ⨱ plateScale).value
      val distance   = BigDecimal(math.sqrt((D._1.pow(2) + D._2.pow(2)).toDouble)).min(scaledPBAL + scaledPPAL)
      val a = (scaledPBAL.pow(2) - scaledPPAL.pow(2) + distance.pow(2)) / (2 * distance)
      val h = (if (port === PortDisposition.Bottom) -1 else 1) *
                math.sqrt((scaledPBAL.pow(2)- a.pow(2)).toDouble)
      val p1 = (a * D._1 + h * D._2) / distance
      val q1 = (a * D._2 - h * D._1) / distance
      val p = -guideStar._1 + P._1 + p1
      val q = -guideStar._2 + P._2 + q1
      (p, q)
    }
    Angle.fromDoubleRadians(-math.atan2(Q._2.toDouble, Q._1.toDouble)).flip
  }

object probeArm extends F2OiwfsProbeArm

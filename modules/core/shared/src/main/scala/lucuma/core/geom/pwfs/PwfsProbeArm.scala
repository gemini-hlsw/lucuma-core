// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.pwfs

import algebra.instances.all.given
import cats.syntax.all.*
import coulomb.*
import coulomb.syntax.*
import coulomb.units.accepted.ArcSecond
import coulomb.units.accepted.Millimeter
import lucuma.core.enums.GuideProbe
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.units.*
import lucuma.core.math.syntax.units.*

import java.lang.Math.PI
import java.lang.Math.acos
import java.lang.Math.atan2
import java.lang.Math.hypot
import java.lang.Math.sqrt

/**
 * PWFS probe arm geometry as in OCS TpePWFSFeature.java.
 */
trait PwfsProbeArm:

  // From PwfsGuideProbe.java
  private val ArmLength: Quantity[BigDecimal, Millimeter] = 400.0.mm
  private val M2Radius: Quantity[BigDecimal, Millimeter] = 511.0.mm
  private val M2FplaneSep: Quantity[BigDecimal, Millimeter] = 16539.326.mm
  private val PlateScale: PlateScale = 1.611.plateScale

  // PWFS1 dimensions
  private val Pwfs1FplaneSep: Quantity[BigDecimal, Millimeter] = 1995.0.mm
  private val Pwfs1MirrorWidth: Quantity[BigDecimal, Millimeter] = 157.0.mm
  private val Pwfs1MirrorLength: Quantity[BigDecimal, Millimeter] = 157.0.mm
  private val Pwfs1ArmWidth: Quantity[BigDecimal, Millimeter] = 202.0.mm

  // PWFS2 dimensions
  private val Pwfs2FplaneSep: Quantity[BigDecimal, Millimeter] = 1610.0.mm
  private val Pwfs2MirrorWidth: Quantity[BigDecimal, Millimeter] = 127.0.mm
  private val Pwfs2MirrorLength: Quantity[BigDecimal, Millimeter] = 127.0.mm
  private val Pwfs2ArmWidth: Quantity[BigDecimal, Millimeter] = 202.0.mm

  private val ZeroMM = 0.0.mm
  private val ZeroArcsec  = 0.0.arcsecs

  // Accessors for probe-specific values
  private def fplaneSep(probe: GuideProbe): Quantity[BigDecimal, Millimeter] =
    probe match
      case GuideProbe.PWFS1 => Pwfs1FplaneSep
      case GuideProbe.PWFS2 => Pwfs2FplaneSep
      case _                => ZeroMM

  private def mirrorWidth(probe: GuideProbe): Quantity[BigDecimal, Millimeter] =
    probe match
      case GuideProbe.PWFS1 => Pwfs1MirrorWidth
      case GuideProbe.PWFS2 => Pwfs2MirrorWidth
      case _                => ZeroMM

  private def mirrorLength(probe: GuideProbe): Quantity[BigDecimal, Millimeter] =
    probe match
      case GuideProbe.PWFS1 => Pwfs1MirrorLength
      case GuideProbe.PWFS2 => Pwfs2MirrorLength
      case _                => ZeroMM

  private def armWidth(probe: GuideProbe): Quantity[BigDecimal, Millimeter] =
    probe match
      case GuideProbe.PWFS1 => Pwfs1ArmWidth
      case GuideProbe.PWFS2 => Pwfs2ArmWidth
      case _                => ZeroMM

  // Derived values - vignetting geometry
  // dr: M2 offset used to create the smooth vignetting region boundary
  private def dr(probe: GuideProbe): Quantity[BigDecimal, Millimeter] =
    M2Radius * fplaneSep(probe) / M2FplaneSep

  // step: Width of the notch (half the difference between arm width and mirror width)
  private def step(probe: GuideProbe): Quantity[BigDecimal, Millimeter] =
    (armWidth(probe) - mirrorWidth(probe)) / 2

  // dxl: X displacement for the rounded notch corner
  private def dxl(probe: GuideProbe): Quantity[BigDecimal, Millimeter] =
    val d = dr(probe).value
    val s = step(probe).value
    sqrt((d * d - (d - s) * (d - s)).toDouble).mm

  // Arm length in arcsec
  private val armLengthArcsec: Quantity[BigDecimal, ArcSecond] = ArmLength ⨱ PlateScale

  // Helper to create P/Q offsets from arcsec Quantities
  private def pq(x: Quantity[BigDecimal, ArcSecond], y: Quantity[BigDecimal, ArcSecond]): (Offset.P, Offset.Q) =
    (x.toAngle.p, y.toAngle.q)

  /**
   * Mirror outline (4 points) - the actual pickoff mirror rectangle.
   * Slightly inset from vignetting boundary by dr.
   */
  def mirror(probe: GuideProbe): ShapeExpression =
    val ml = mirrorLength(probe)
    val mw = mirrorWidth(probe)
    val d = dr(probe)

    // Mirror centered at origin
    val x1 = (ml / 2 - d) ⨱ PlateScale
    val y1 = (-mw / 2 + d) ⨱ PlateScale
    val x2 = x1
    val y2 = -y1
    val x3 = (-ml/ 2 + d) ⨱ PlateScale
    val y3 = y2
    val x4 = x3
    val y4 = y1

    ShapeExpression.polygonAt(
      pq(x1, y1),
      pq(x2, y2),
      pq(x3, y3),
      pq(x4, y4)
    )

  /**
   * Partially vignetted region around the mirror (10 points).
   * This is the outer halo with rounded corners.
   */
  def partiallyVignettedMirror(probe: GuideProbe): ShapeExpression =
    val ml = mirrorLength(probe)
    val mw = mirrorWidth(probe)
    val d = dr(probe)
    val dx = dxl(probe)
    val sqrt2 = BigDecimal(math.sqrt(2.0))

    // Mirror centered at origin, arm extends in -X direction
    // Point 1: tip of mirror region on axis
    val x1 = (ml / 2 + d) ⨱ PlateScale
    val y1 = ZeroArcsec

    // Point 2: corner at mirror width
    val x2 = x1
    val y2 = (mw / 2) ⨱ PlateScale

    // Point 3: 45 degree point on arc
    val x3 = (ml/ 2 + d / sqrt2) ⨱ PlateScale
    val y3 = y2 + (d / sqrt2) ⨱ PlateScale

    // Point 4: 90 degree point on arc
    val x4 = (ml / 2) ⨱ PlateScale
    val y4 = y2 + d ⨱ PlateScale

    // Point 5: notch transition point
    val x5 = (-ml / 2 + dx) ⨱ PlateScale
    val y5 = y4

    // Points 6-10: reflection through y axis
    ShapeExpression.polygonAt(
      pq(x1, y1),
      pq(x2, y2),
      pq(x3, y3),
      pq(x4, y4),
      pq(x5, y5),
      pq(x5, -y5),
      pq(x4, -y4),
      pq(x3, -y3),
      pq(x2, -y2),
      pq(x1, -y1)
    )

  /**
   * Arm shape - one half (upper or lower based on sign).
   * Extends from the notch back to the pivot.
   */
  private def armHalf(probe: GuideProbe, sign: BigDecimal): ShapeExpression =
    val ml = mirrorLength(probe)
    val mw = mirrorWidth(probe)
    val aw = armWidth(probe)
    val d = dr(probe)
    val dx = dxl(probe)
    val r = armLengthArcsec

    // Mirror at origin, arm extends in -X direction toward pivot at -r
    // Starting point at notch
    val x1 = (-ml / 2 + dx) ⨱ PlateScale
    val y1 = ZeroArcsec

    val x2 = x1
    val y2 = (mw / 2) ⨱ PlateScale + d ⨱ PlateScale

    val x3 = (-ml / 2) ⨱ PlateScale
    val y3 = (aw / 2 + d) ⨱ PlateScale

    // Intermediate point for PWFS2
    val (x23, y23) = if (probe == GuideProbe.PWFS2)
      (((x2 + x3) / 2), ((y2 + y3) / 2))
    else (ZeroArcsec, ZeroArcsec)

    // Point along arm
    val x4 = x3 - d ⨱ PlateScale
    val y4 = y3

    // End at pivot (now at -r from mirror)
    val x5 = -r
    val y5 = y3

    val x6 = -r
    val y6 = ZeroArcsec

    val x7 = x1
    val y7 = y1

    val points = if (probe === GuideProbe.PWFS2)
      List(
        pq(x1, (sign * y1)),
        pq(x2, (sign * y2)),
        pq(x23, (sign * y23)),
        pq(x3, (sign * y3)),
        pq(x4, (sign * y4)),
        pq(x5, (sign * y5)),
        pq(x6, (sign * y6)),
        pq(x7, (sign * y7))
      )
    else
      List(
        pq(x1, (sign * y1)),
        pq(x2, (sign * y2)),
        pq(x3, (sign * y3)),
        pq(x4, (sign * y4)),
        pq(x5, (sign * y5)),
        pq(x6, (sign * y6)),
        pq(x7, (sign * y7)),
      )

    ShapeExpression.polygonAt(points*)

  private def armUpperHalf(probe: GuideProbe): ShapeExpression = armHalf(probe, 1.0)
  private def armLowerHalf(probe: GuideProbe): ShapeExpression = armHalf(probe, -1.0)

  /**
   * Fully vignetted arm region (14 points).
   * The inner part of the arm that's completely blocked.
   */
  def fullyVignettedArm(probe: GuideProbe): ShapeExpression =
    val ml = mirrorLength(probe)
    val mw = mirrorWidth(probe)
    val aw = armWidth(probe)
    val d = dr(probe)
    val dx = dxl(probe)
    val r = armLengthArcsec

    // Mirror at origin, arm extends toward pivot at -r
    val x1 = (-ml / 2 + d) ⨱ PlateScale
    val y1 = ZeroArcsec

    val x2 = x1
    val y2 = (mw / 2 - d) ⨱ PlateScale

    val x3 = (-ml / 2) ⨱ PlateScale
    val y3 = y2

    val x6 = x3 - dx ⨱ PlateScale
    val y6 = (aw / 2 - d) ⨱ PlateScale

    // Intermediate points for smooth curve
    val (x4, y4, x5, y5) = if (probe === GuideProbe.PWFS1) {
      val theta = math.asin((dx / d).value.toDouble) / 3.0
      (
        x3 - (d * math.sin(theta)) ⨱ PlateScale,
        y3 + (d * (1.0 - math.cos(theta))) ⨱ PlateScale,
        x3 - (d * math.sin(2.0 * theta)) ⨱ PlateScale,
        y3 + (d * (1.0 - math.cos(2.0 * theta))) ⨱ PlateScale
      )
    } else {
      (
        (x3 + (x6 - x3) / 3),
        (y3 + (y6 - y3) / 3),
        (x3 + BigDecimal(2) * (x6 - x3) / 3),
        (y3 + BigDecimal(2) * (y6 - y3) / 3)
      )
    }

    val x7 = -r
    val y7 = y6

    ShapeExpression.polygonAt(
      pq(x1, y1),
      pq(x2, y2),
      pq(x3, y3),
      pq(x4, y4),
      pq(x5, y5),
      pq(x6, y6),
      pq(x7, y7),
      // Reflect through y axis
      pq(x7, -y7),
      pq(x6, -y6),
      pq(x5, -y5),
      pq(x4, -y4),
      pq(x3, -y3),
      pq(x2, -y2),
      pq(x1, -y1)
    )

  private def vignetteShape(probe: GuideProbe): ShapeExpression =
    partiallyVignettedMirror(probe) ∪
      armUpperHalf(probe) ∪
      armLowerHalf(probe) ∪
      fullyVignettedArm(probe)

  /**
   * Calculate arm angle to reach guide star as ocs in TpePWFSFeature.java.
   */
  private def armAngle(guideStar: Offset, offsetPos: Offset): Angle =
    val (p, q) =
      Offset.signedDecimalArcseconds
        .get(guideStar - offsetPos)
        .bimap(_.toDouble, _.toDouble)

    val s2 = hypot(p, q) / 2.0

    // Bearing of guide star
    val b = atan2(-q, p)

    val armLen = armLengthArcsec.value.toDouble

    // Angle offset for pivot geometry
    val d =
      if (s2 > 0 && armLen > 0) {
        val ratio = s2 / armLen
        if (ratio >= 1.0) 0.0
        else if (ratio <= -1.0) PI
        else acos(ratio)
      } else
        0.0

    Angle.fromDoubleRadians(b - d)

  /**
   * Shape of all the area vignetted, union of mirror and argm mignettes
   */
  def vignettedAreaAt(
    probe:     GuideProbe,
    guideStar: Offset,
    offsetPos: Offset
  ): ShapeExpression =
    val angle = armAngle(guideStar, offsetPos)
    vignetteShape(probe) ⟲ angle ↗ guideStar

  /**
   * Positioned mirror outline at guide star location.
   */
  def mirrorAt(
    probe:     GuideProbe,
    guideStar: Offset,
    offsetPos: Offset
  ): ShapeExpression =
    val angle = armAngle(guideStar, offsetPos)
    mirror(probe) ⟲ angle ↗ guideStar

  /**
   * Positioned partially vignetted region at guide star location.
   */
  def mirrorVignettedAreaAt(
    probe:     GuideProbe,
    guideStar: Offset,
    offsetPos: Offset
  ): ShapeExpression =
    val angle = armAngle(guideStar, offsetPos)
    partiallyVignettedMirror(probe) ⟲ angle ↗ guideStar

  /**
   * Arm shape including the partial shadow inducing vignetting
   */
  def armVignettedAreaAt(
    probe:     GuideProbe,
    guideStar: Offset,
    offsetPos: Offset
  ): ShapeExpression =
    val angle = armAngle(guideStar, offsetPos)
    val arm = armUpperHalf(probe) ∪ armLowerHalf(probe)
    arm ⟲ angle ↗ guideStar

  /**
   * Arm shape, fully vignetting
   */
  def armAt(
    probe:     GuideProbe,
    guideStar: Offset,
    offsetPos: Offset
  ): ShapeExpression =
    val angle = armAngle(guideStar, offsetPos)
    fullyVignettedArm(probe) ⟲ angle ↗ guideStar

object probeArm extends PwfsProbeArm

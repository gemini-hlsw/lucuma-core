// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.pwfs

import cats.syntax.all.*
import lucuma.core.enums.GuideProbe
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset

import java.lang.Math.PI
import java.lang.Math.acos
import java.lang.Math.atan2
import java.lang.Math.hypot
import java.lang.Math.sqrt

/**
 * PWFS probe arm geometry matching OCS TpePWFSFeature.java exactly.
 * Includes vignetting regions around the arm.
 */
trait PwfsProbeArm:

  // OCS constants (mm) from PwfsGuideProbe.java
  private val ArmLengthMm: Double = 400.0
  private val M2Radius: Double = 511.0
  private val M2FplaneSep: Double = 16539.326
  private val MmToArcsec: Double = 1.611

  // PWFS1 dimensions (mm)
  private val Pwfs1FplaneSep: Double = 1995.0
  private val Pwfs1MirrorWidthMm: Double = 157.0
  private val Pwfs1MirrorLengthMm: Double = 157.0
  private val Pwfs1ArmWidthMm: Double = 202.0

  // PWFS2 dimensions (mm)
  private val Pwfs2FplaneSep: Double = 1610.0
  private val Pwfs2MirrorWidthMm: Double = 127.0
  private val Pwfs2MirrorLengthMm: Double = 127.0
  private val Pwfs2ArmWidthMm: Double = 202.0

  // Accessors for probe-specific values (mm)
  private def fplaneSepMm(probe: GuideProbe): Double = probe match
    case GuideProbe.PWFS1 => Pwfs1FplaneSep
    case GuideProbe.PWFS2 => Pwfs2FplaneSep
    case _                => 0.0

  private def mirrorWidthMm(probe: GuideProbe): Double = probe match
    case GuideProbe.PWFS1 => Pwfs1MirrorWidthMm
    case GuideProbe.PWFS2 => Pwfs2MirrorWidthMm
    case _                => 0.0

  private def mirrorLengthMm(probe: GuideProbe): Double = probe match
    case GuideProbe.PWFS1 => Pwfs1MirrorLengthMm
    case GuideProbe.PWFS2 => Pwfs2MirrorLengthMm
    case _                => 0.0

  private def armWidthMm(probe: GuideProbe): Double = probe match
    case GuideProbe.PWFS1 => Pwfs1ArmWidthMm
    case GuideProbe.PWFS2 => Pwfs2ArmWidthMm
    case _                => 0.0

  // Derived values (mm) - vignetting geometry
  // dr: M2 offset used to create the smooth vignetting region boundary
  private def drMm(probe: GuideProbe): Double =
    M2Radius * fplaneSepMm(probe) / M2FplaneSep

  // step: Width of the notch (half the difference between arm width and mirror width)
  private def stepMm(probe: GuideProbe): Double =
    (armWidthMm(probe) - mirrorWidthMm(probe)) / 2.0

  // dxl: X displacement for the rounded notch corner
  private def dxlMm(probe: GuideProbe): Double =
    val d = drMm(probe)
    val s = stepMm(probe)
    sqrt(d * d - (d - s) * (d - s))

  // Arm length in arcsec (r in OCS)
  private val armLengthArcsec: Double = ArmLengthMm * MmToArcsec

  // Convert mm to arcsec for a given value
  private def toArcsec(mm: Double): Double = mm * MmToArcsec

  // Helper to create P/Q offsets from arcsec doubles
  private def pq(x: Double, y: Double): (Offset.P, Offset.Q) =
    (Angle.fromDoubleArcseconds(x).p, Angle.fromDoubleArcseconds(y).q)

  /**
   * Mirror outline (4 points) - the actual pickoff mirror rectangle.
   * Slightly inset from vignetting boundary by dr.
   */
  def mirror(probe: GuideProbe): ShapeExpression =
    val ml = mirrorLengthMm(probe)
    val mw = mirrorWidthMm(probe)
    val dr = drMm(probe)
    val r = armLengthArcsec

    val x1 = toArcsec(ml / 2.0 - dr) + r
    val y1 = toArcsec(-mw / 2.0 + dr)
    val x2 = x1
    val y2 = -y1
    val x3 = toArcsec(-ml / 2.0 + dr) + r
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
    val ml = mirrorLengthMm(probe)
    val mw = mirrorWidthMm(probe)
    val dr = drMm(probe)
    val dxl = dxlMm(probe)
    val r = armLengthArcsec
    val sqrt2 = math.sqrt(2.0)

    // Point 1: tip of mirror region on axis
    val x1 = toArcsec(ml / 2.0 + dr) + r
    val y1 = 0.0

    // Point 2: corner at mirror width
    val x2 = x1
    val y2 = toArcsec(mw / 2.0)

    // Point 3: 45 degree point on arc
    val x3 = toArcsec(ml / 2.0 + dr / sqrt2) + r
    val y3 = y2 + toArcsec(dr / sqrt2)

    // Point 4: 90 degree point on arc
    val x4 = toArcsec(ml / 2.0) + r
    val y4 = y2 + toArcsec(dr)

    // Point 5: notch transition point
    val x5 = toArcsec(-ml / 2.0 + dxl) + r
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
  private def armHalf(probe: GuideProbe, sign: Double): ShapeExpression =
    val ml = mirrorLengthMm(probe)
    val mw = mirrorWidthMm(probe)
    val aw = armWidthMm(probe)
    val dr = drMm(probe)
    val dxl = dxlMm(probe)
    val r = armLengthArcsec

    // Starting point at notch
    val x1 = toArcsec(-ml / 2.0 + dxl) + r
    val y1 = 0.0

    val x2 = x1
    val y2 = toArcsec(mw / 2.0) + toArcsec(dr)

    val x3 = toArcsec(-ml / 2.0) + r
    val y3 = toArcsec(aw / 2.0 + dr)

    // Intermediate point for PWFS2
    val (x23, y23) = if (probe == GuideProbe.PWFS2)
      ((x2 + x3) / 2.0, (y2 + y3) / 2.0)
    else (0.0, 0.0)

    // Point along arm
    val x4 = x3 - toArcsec(dr)
    val y4 = y3

    // End at pivot
    val x5 = 0.0
    val y5 = y3

    val x6 = 0.0
    val y6 = 0.0

    val x7 = x1
    val y7 = y1

    val points = if (probe == GuideProbe.PWFS2)
      List(
        pq(x1, sign * y1),
        pq(x2, sign * y2),
        pq(x23, sign * y23),
        pq(x3, sign * y3),
        pq(x4, sign * y4),
        pq(x5, sign * y5),
        pq(x6, sign * y6),
        pq(x7, sign * y7)
      )
    else
      List(
        pq(x1, sign * y1),
        pq(x2, sign * y2),
        pq(x3, sign * y3),
        pq(x4, sign * y4),
        pq(x5, sign * y5),
        pq(x6, sign * y6),
        pq(x7, sign * y7)
      )

    ShapeExpression.polygonAt(points*)

  def armUpperHalf(probe: GuideProbe): ShapeExpression = armHalf(probe, 1.0)
  def armLowerHalf(probe: GuideProbe): ShapeExpression = armHalf(probe, -1.0)

  /**
   * Fully vignetted arm region (14 points).
   * The inner part of the arm that's completely blocked.
   */
  def fullyVignettedArm(probe: GuideProbe): ShapeExpression =
    val ml = mirrorLengthMm(probe)
    val mw = mirrorWidthMm(probe)
    val aw = armWidthMm(probe)
    val dr = drMm(probe)
    val dxl = dxlMm(probe)
    val r = armLengthArcsec

    val x1 = toArcsec(-ml / 2.0 + dr) + r
    val y1 = 0.0

    val x2 = x1
    val y2 = toArcsec(mw / 2.0 - dr)

    val x3 = toArcsec(-ml / 2.0) + r
    val y3 = y2

    val x6 = x3 - toArcsec(dxl)
    val y6 = toArcsec(aw / 2.0 - dr)

    // Intermediate points for smooth curve
    val (x4, y4, x5, y5) = if (probe == GuideProbe.PWFS1) {
      val theta = math.asin(dxl / dr) / 3.0
      (
        x3 - toArcsec(dr * math.sin(theta)),
        y3 + toArcsec(dr * (1.0 - math.cos(theta))),
        x3 - toArcsec(dr * math.sin(2.0 * theta)),
        y3 + toArcsec(dr * (1.0 - math.cos(2.0 * theta)))
      )
    } else {
      (
        x3 + (x6 - x3) / 3.0,
        y3 + (y6 - y3) / 3.0,
        x3 + 2.0 * (x6 - x3) / 3.0,
        y3 + 2.0 * (y6 - y3) / 3.0
      )
    }

    val x7 = 0.0
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

  /**
   * Combined vignetting shape - union of all arm regions.
   */
  def vignetteShape(probe: GuideProbe): ShapeExpression =
    partiallyVignettedMirror(probe) ∪
      armUpperHalf(probe) ∪
      armLowerHalf(probe) ∪
      fullyVignettedArm(probe)

  /**
   * Calculate arm angle to reach guide star.
   * Matches OCS calculation in TpePWFSFeature.java.
   */
  private def armAngle(guideStar: Offset, offsetPos: Offset): Angle =
    val (p, q) = Offset.signedDecimalArcseconds
      .get(guideStar - offsetPos)
      .bimap(_.toDouble, _.toDouble)

    // Half distance to guide star
    val s2 = hypot(p, q) / 2.0

    // Bearing of guide star (OCS uses atan2(-dy, dx))
    val b = atan2(-q, p)

    // Angle offset for pivot geometry
    val d = if (s2 > 0 && armLengthArcsec > 0) {
      val ratio = s2 / armLengthArcsec
      if (ratio >= 1.0) 0.0
      else if (ratio <= -1.0) PI
      else acos(ratio)
    } else 0.0

    Angle.fromDoubleRadians(b + d)

  /**
   * Position the vignette shape at a guide star location.
   */
  def shapeAt(
    probe:     GuideProbe,
    guideStar: Offset,
    offsetPos: Offset
  ): ShapeExpression =
    val angle = armAngle(guideStar, offsetPos)
    vignetteShape(probe) ⟲ angle ↗ guideStar

object probeArm extends PwfsProbeArm

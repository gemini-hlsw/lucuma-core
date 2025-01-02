// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.f2

import cats.syntax.all.*
import coulomb.*
import coulomb.conversion.UnitConversion
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import coulomb.units.accepted.*
import coulomb.units.si.prefixes.*
import lucuma.core.enums.F2LyotWheel
import lucuma.core.enums.PortDisposition
import lucuma.core.geom.*
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*
import lucuma.core.math.units.*
import spire.std.bigDecimal.*

// import scala.language.implicitConversions

/**
  * F2 area that could be reachable by the patrol arm
  * https://www.gemini.edu/instrumentation/flamingos-2/capability
  */
trait F2PatrolField {

  /**
    * GMOS area where the probe arm can reach centered at 0
    */
  def candidatesArea: ShapeExpression =
    // 4.9 arcmin radius
    // NOTE There is some debate on whether this should be 4.8
    ShapeExpression.centeredEllipse((4.9 * 60 * 2).toInt.arcsec,
                                    (4.9 * 60 * 2).toInt.arcsec
    )

  /**
    * GMOS area where the probe arm can reach centered with a given posAngle and offset
    */
  def candidatesAreaAt(posAngle: Angle, offsetPos: Offset): ShapeExpression =
    candidatesArea ↗ offsetPos ⟲ posAngle

  /**
    * GMOS area reachable by the problem arm for a set of posAngles and offsets
    */
  def candidatesAreaAt(posAngles: List[Angle], offsetPositions: List[Offset]): ShapeExpression =
    (for {
      a <- posAngles
      o <- offsetPositions
    } yield candidatesAreaAt(a, o)).fold(ShapeExpression.Empty)(_ ∪ _)

  /**
    * F2 patrol field shape centered at the base position.
    */
  def patrolField(plateScale: Quantity[BigDecimal, ArcSecondPerMillimeter]): ShapeExpression =
    // define the "upper" and "lower" half-circles defining the patrol are
    // -- use full circle for upper smaller one (using only half circle for upper part of figure can
    // -- end in two disjoint areas due to calculation imprecisions, we have to make sure areas overlap
    // -- properly to yield the figure we want).
    val Two = BigDecimal(2)
    val ZeroM = BigDecimal(0).withUnit[Micrometer]
    val upperPAOffset = (-PickOffPivotPoint, ZeroM).withPlateScaleOffset(plateScale)
    val upperPA = ShapeExpression.centeredEllipse(
                (UpperPatrolAreaRadius * Two).withPlateScale(plateScale).toAngle,
                (UpperPatrolAreaRadius * Two).withPlateScale(plateScale).toAngle)

    val lowerPAOffset = (-BasePivotPoint, ZeroM).withPlateScaleOffset(plateScale)
    val lowerPA = ShapeExpression.centeredClosedArc(
                (LowerPatrolAreaRadius * Two).withPlateScale(plateScale).toAngle,
                (LowerPatrolAreaRadius * Two).withPlateScale(plateScale).toAngle,
                Angle.Angle0,
                Angle.Angle180)

    // define the two bounding shapes (one circle and a box)
    val ew = ShapeExpression.centeredEllipse(
                (EntranceWindowRadius * Two).withPlateScale(plateScale).toAngle,
                (EntranceWindowRadius * Two).withPlateScale(plateScale).toAngle
              )

    val paLimitOffset = ((EntranceWindowRadius - PatrolAreaHiLimit), ZeroM).withPlateScaleOffset(plateScale)
    val paLimit = ShapeExpression.centeredRectangle(
                (EntranceWindowRadius + PatrolAreaHiLimit).withPlateScale(plateScale).toAngle,
                  (EntranceWindowRadius * Two).withPlateScale(plateScale).toAngle)

    (((upperPA ↗ upperPAOffset) ∪ (lowerPA ↗ lowerPAOffset)) ∩ ew ) ∩ (paLimit ↗ paLimitOffset)

  /**
    * F2 patrol field shape, in context.
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
    lyotWheel: F2LyotWheel,
    port:      PortDisposition
  ): ShapeExpression = {
    val plateScale = BigDecimal(lyotWheel.plateScale).withUnit[ArcSecondPerMillimeter]
    val pf = patrolField(plateScale)
    val s  = if (port === PortDisposition.Side) pf.flipQ else pf
    s ↗ offsetPos ⟲ posAngle
  }

}

object patrolField extends F2PatrolField

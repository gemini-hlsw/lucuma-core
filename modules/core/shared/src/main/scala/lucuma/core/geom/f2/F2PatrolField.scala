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
import lucuma.core.math.units.*
import spire.std.bigDecimal.*

/**
  * F2 area that could be reachable by the patrol arm
  * https://www.gemini.edu/instrumentation/flamingos-2/capability
  */
trait F2PatrolField:
  val Two = BigDecimal(2)
  val ZeroM = BigDecimal(0).withUnit[Micrometer]

  /**
    * F2 patrol field shape centered at the base position.
    */
  def patrolField(plateScale: F2PlateScale): ShapeExpression = {
    // define the "upper" and "lower" half-circles defining the patrol are
    // -- use full circle for upper smaller one (using only half circle for upper part of figure can
    // -- end in two disjoint areas due to calculation imprecisions, we have to make sure areas overlap
    // -- properly to yield the figure we want).
    val upperPAOffset = (-PickOffPivotPoint, ZeroM) ⤇ plateScale
    val upperPA =
      ShapeExpression.centeredEllipse(
        ((UpperPatrolAreaRadius * Two) ⨱ plateScale).toAngle,
        ((UpperPatrolAreaRadius * Two) ⨱ plateScale).toAngle
      ) ↗ upperPAOffset

    val lowerPAOffset = (-BasePivotPoint, ZeroM) ⤇ plateScale
    val lowerPA =
      ShapeExpression.centeredClosedArc(
        ((LowerPatrolAreaRadius * Two) ⨱ plateScale).toAngle,
        ((LowerPatrolAreaRadius * Two) ⨱ plateScale).toAngle,
        Angle.Angle0,
        Angle.Angle180
      ) ↗ lowerPAOffset

    // define the two bounding shapes (one circle and a box)
    val ew =
      ShapeExpression.centeredEllipse(
        ((EntranceWindowRadius * Two) ⨱ plateScale).toAngle,
        ((EntranceWindowRadius * Two) ⨱ plateScale).toAngle
      )

    val paLimitOffset =
      ((EntranceWindowRadius-PatrolAreaHiLimit)/2, ZeroM) ⤇ plateScale

    val paLimit =
      ShapeExpression.centeredRectangle(
        ((EntranceWindowRadius + PatrolAreaHiLimit) ⨱ plateScale).toAngle,
        ((EntranceWindowRadius * Two) ⨱ plateScale).toAngle
      )↗  paLimitOffset

    ((upperPA ∪ lowerPA) ∩ ew) ∩ paLimit
  }

  /**
    * F2 patrol field shape, in context.
    *
    * @param posAngle position angle where positive is counterclockwise
    * @param offsetPos offset position from the base, if any
    * @param lyot Lyot Wheel position, to get the plate scale
    * @param port port disposition
    *
    * @return probe field shape rotated and offset
    */
  def patrolFieldAt(
    posAngle:  Angle,
    offsetPos: Offset,
    lyotWheel: F2LyotWheel,
    port:      PortDisposition
  ): ShapeExpression =
    val plateScale = BigDecimal(lyotWheel.plateScale).withUnit[ArcSecondPerMillimeter]
    val pf = patrolField(plateScale)
    val s  = if (port === PortDisposition.Bottom) pf.flipP else pf
    s ↗ offsetPos ⟲ posAngle

object patrolField extends F2PatrolField

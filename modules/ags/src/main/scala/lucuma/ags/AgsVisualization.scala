// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.Order
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import lucuma.core.geom.Area
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.offsets.OffsetPosition
import lucuma.core.geom.offsets.RotatedOffset
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset

final case class PatrolFieldVisualization(
  position:       OffsetPosition,
  posPatrolField: ShapeExpression, // Patrol field for the position
  paIntersection: ShapeExpression, // Intersection area for the pa
  rotatedOffset:  RotatedOffset    // Position for placing visualization markers
)

final case class ScienceOverlapVisualization(
  position:               OffsetPosition,
  guideStarOffset:        Offset,
  probeArmShape:          ShapeExpression,
  scienceTargetAreaShape: ShapeExpression,
  scienceAreaShape:       ShapeExpression,
  targetOverlap:          ShapeExpression,
  detectorVignetting:     ShapeExpression,
  overlapsScience:        Boolean,
  vignettingArea:         Area
)

object AgsVisualization {

  def patrolFieldGeometries(
    params:    SingleProbeAgsParams,
    positions: NonEmptyList[OffsetPosition]
  ): NonEmptyList[PatrolFieldVisualization] = {
    given Order[Angle] = Angle.SignedAngleOrder

    val calcsByPA: Map[Angle, NonEmptyMap[OffsetPosition, AgsGeomCalc]] =
      positions
        .groupBy(_.posAngle)
        .view
        .mapValues(params.posCalculations)
        .toMap

    // Helper to get patrol field
    def getPatrolField(position: OffsetPosition): ShapeExpression =
      params.patrolFieldAt(position.posAngle, position.offsetPos, position.pivot)

    positions.map: position =>
      PatrolFieldVisualization(
        position = position,
        posPatrolField = getPatrolField(position),
        paIntersection = calcsByPA
          .get(position.posAngle)
          .flatMap(_.apply(position))
          .map(_.intersectionPatrolField)
          .getOrElse(ShapeExpression.Empty),
        rotatedOffset = position.rotatedOffset
      )
  }

  def scienceOverlapVisualization(
    params:          SingleProbeAgsParams,
    position:        OffsetPosition,
    guideStarOffset: Offset
  ): ScienceOverlapVisualization = {
    val probeArm = params.probeArm(position.posAngle, guideStarOffset, position.offsetPos)

    val scienceTargetArea =
      ShapeExpression
        .centeredEllipse(params.scienceRadius,
                         params.scienceRadius
        ) ↗ position.offsetPos ⟲ position.posAngle

    val scienceArea = params.scienceArea(position.posAngle, position.offsetPos)

    val targetOverlap      = probeArm ∩ scienceTargetArea
    val detectorVignetting = probeArm ∩ scienceArea

    // Compute metrics
    val overlaps      = targetOverlap.maxSide.toMicroarcseconds > 5
    val vignettedArea = detectorVignetting.eval.area

    ScienceOverlapVisualization(
      position = position,
      guideStarOffset = guideStarOffset,
      probeArmShape = probeArm,
      scienceTargetAreaShape = scienceTargetArea,
      scienceAreaShape = scienceArea,
      targetOverlap = targetOverlap,
      detectorVignetting = detectorVignetting,
      overlapsScience = overlaps,
      vignettingArea = vignettedArea
    )
  }

}

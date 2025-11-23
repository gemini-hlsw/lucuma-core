// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import lucuma.core.geom.Area
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset

case class PatrolFieldVisualization(
  geometryType:   GeometryType,
  position:       AgsPosition,
  posPatrolField: ShapeExpression, // Patrol field for the position
  paIntersection: ShapeExpression  // Intersection area for the pa
)

case class ScienceOverlapVisualization(
  position:               AgsPosition,
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
    positions: NonEmptyList[(GeometryType, AgsPosition)]
  ): NonEmptyList[PatrolFieldVisualization] = {

    // Calculate intersection of all patrol fields at each position angle
    val intersectionsByPA: Map[Angle, NonEmptyMap[AgsPosition, ShapeExpression]] =
      positions
        .groupBy(_._2.posAngle)
        .view
        .mapValues: positionsAtPA =>
          params.posCalculations(positionsAtPA.map(_._2)).map(_.intersectionPatrolField)
        .toMap

    positions.map: (gt, position) =>
      PatrolFieldVisualization(
        geometryType = gt,
        position = position,
        posPatrolField = params.patrolFieldAt(position.posAngle, position.offsetPos),
        paIntersection = intersectionsByPA
          .get(position.posAngle)
          .flatMap(_.apply(position))
          .getOrElse(ShapeExpression.Empty)
      )
  }

  def scienceOverlapVisualization(
    params:          SingleProbeAgsParams,
    position:        AgsPosition,
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

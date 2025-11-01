// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.data.NonEmptyList
import cats.syntax.all.*
import lucuma.core.geom.Area
import lucuma.core.geom.ShapeExpression
import lucuma.core.math.Angle
import lucuma.core.math.Offset

/**
 * Visualization data pairing a position with its individual patrol field and the intersected patrol
 * field at that position angle.
 */
case class PatrolFieldVisualization(
  position:               AgsPosition,
  individualPatrolField:  ShapeExpression,
  intersectedPatrolField: ShapeExpression
)

/**
 * Viz data showing probe arm geometry and its overlaps with science areas.
 */
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

  def patrolFieldVisualization(
    params:    SingleProbeAgsParams,
    positions: NonEmptyList[AgsPosition]
  ): NonEmptyList[PatrolFieldVisualization] = {

    // Calculate intersection of all patrol fields at each position angle
    val intersectionsByPA: Map[Angle, ShapeExpression] =
      positions
        .groupBy(_.posAngle)
        .view
        .mapValues { positionsAtPA =>
          positionsAtPA
            .map(_.offsetPos)
            .distinct
            .map(offset => params.patrolFieldAt(positionsAtPA.head.posAngle, offset))
            .reduce(using _ ∩ _)
        }
        .toMap

    positions.map { position =>
      PatrolFieldVisualization(
        position = position,
        individualPatrolField = params.patrolFieldAt(position.posAngle, position.offsetPos),
        intersectedPatrolField = intersectionsByPA(position.posAngle)
      )
    }
  }

  def scienceOverlapVisualization(
    params:          SingleProbeAgsParams,
    position:        AgsPosition,
    guideStarOffset: Offset
  ): ScienceOverlapVisualization = {

    // Get the probe arm geometry at the guide star position
    val probeArm = params.probeArm(position.posAngle, guideStarOffset, position.offsetPos)

    // Science target area (20 arcsec radius circle)
    val scienceTargetArea =
      ShapeExpression.centeredEllipse(params.scienceRadius,
                                      params.scienceRadius
      ) ↗ position.offsetPos ⟲ position.posAngle

    // Full detector science area (instrument/FPU dependent)
    val scienceArea = params.scienceArea(position.posAngle, position.offsetPos)

    // Compute intersections
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

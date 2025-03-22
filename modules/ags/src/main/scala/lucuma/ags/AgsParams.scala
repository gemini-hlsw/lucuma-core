// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.Eq
import cats.Order
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.derived.*
import lucuma.core.enums.F2LyotWheel
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.PortDisposition
import lucuma.core.geom.Area
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*
import lucuma.core.model.sequence.f2.F2FpuMask

private given Order[Angle] = Angle.SignedAngleOrder

case class AgsPosition(posAngle: Angle, offsetPos: Offset) derives Order

sealed trait AgsGeomCalc:
  // Indicates if the given offset is reachable
  def isReachable(gsOffset: Offset): Boolean

  // Calculates the area vignetted at a given offset
  def vignettingArea(gsOffset: Offset): Area

  // Indicates if the given guide star would vignette the science target
  def overlapsScience(gsOffset: Offset): Boolean

trait SingleProbeAgsParams:
  def patrolFieldAt(posAngle: Angle, offset: Offset): ShapeExpression

  def scienceArea(posAngle: Angle, offset: Offset): ShapeExpression

  def probeArm(posAngle: Angle, guideStar: Offset, offset: Offset): ShapeExpression

  def scienceRadius: Angle

  def posCalculations(
    positions: NonEmptyList[AgsPosition]
  ): NonEmptyMap[AgsPosition, AgsGeomCalc] =
    val result = positions.map { position =>
      position -> new AgsGeomCalc() {
        private val intersectionPatrolField =
          positions
            .map(_.offsetPos)
            .distinct
            // note we use the outer posAngle but the inner offset
            // we want the intersection of offsets at a single PA
            .map(offset => patrolFieldAt(position.posAngle, offset))
            .reduce(_ ∩ _)
            .eval

        private val scienceAreaShape =
          scienceArea(position.posAngle, position.offsetPos)

        private val scienceTargetArea =
          ShapeExpression.centeredEllipse(scienceRadius,
                                          scienceRadius
          ) ↗ position.offsetPos ⟲ position.posAngle

        override def isReachable(gsOffset: Offset): Boolean =
          intersectionPatrolField.contains(gsOffset)

        def overlapsScience(gsOffset: Offset): Boolean =
          // Calculating with area maybe more precise but it is more costly
          (probeArm(position.posAngle, gsOffset, position.offsetPos)
            ∩ scienceTargetArea).maxSide.toMicroarcseconds > 5

        override def vignettingArea(gsOffset: Offset): Area =
          (scienceAreaShape ∩
            probeArm(position.posAngle, gsOffset, position.offsetPos)).eval.area

      }
    }
    result.toNem

sealed trait AgsParams derives Eq:

  def probe: GuideProbe

  // Builds an AgsGeom object for each position
  // Some of the geometries don't chage with the position and we can cache them
  def posCalculations(positions: NonEmptyList[AgsPosition]): NonEmptyMap[AgsPosition, AgsGeomCalc]

object AgsParams:
  case class GmosAgsParams(
    fpu:  Option[Either[GmosNorthFpu, GmosSouthFpu]],
    port: PortDisposition
  ) extends AgsParams
      with SingleProbeAgsParams derives Eq:
    import lucuma.core.geom.{gmos => GmosGeom}

    val GmosScienceRadius = 20.arcseconds

    override val probe = GuideProbe.GmosOIWFS

    override def patrolFieldAt(posAngle: Angle, offset: Offset): ShapeExpression =
      GmosGeom.probeArm.patrolFieldAt(posAngle, offset, fpu, port)

    override def scienceArea(posAngle: Angle, offset: Offset): ShapeExpression =
      GmosGeom.scienceArea.shapeAt(posAngle, offset, fpu)

    override def probeArm(posAngle: Angle, guideStar: Offset, offset: Offset): ShapeExpression =
      GmosGeom.probeArm.shapeAt(posAngle, guideStar, offset, fpu, port)

    override def scienceRadius: Angle = GmosScienceRadius

  case class F2AgsParams(
    lyot: F2LyotWheel,
    fpu:  F2FpuMask,
    port: PortDisposition
  ) extends AgsParams
      with SingleProbeAgsParams derives Eq:
    import lucuma.core.geom.{f2 => F2Geom}

    val F2ScienceRadius = 20.arcseconds

    override val probe = GuideProbe.F2OIWFS

    override def patrolFieldAt(posAngle: Angle, offset: Offset): ShapeExpression =
      F2Geom.patrolField.patrolFieldAt(posAngle, offset, lyot, port)

    override def scienceArea(posAngle: Angle, offset: Offset): ShapeExpression =
      F2Geom.scienceArea.shapeAt(posAngle, offset, lyot, fpu)

    override def probeArm(posAngle: Angle, guideStar: Offset, offset: Offset): ShapeExpression =
      F2Geom.probeArm.shapeAt(posAngle, guideStar, offset, lyot, port)

    override def scienceRadius: Angle = F2ScienceRadius

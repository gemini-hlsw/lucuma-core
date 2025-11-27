// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.Eq
import cats.Order
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.derived.*
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.PortDisposition
import lucuma.core.geom.Area
import lucuma.core.geom.Shape
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask

private given Order[Angle] = Angle.SignedAngleOrder

case class AgsPosition(
  geometryType: GeometryType,
  posAngle:     Angle,
  offsetPos:    Offset,
  pivot:        Offset = Offset.Zero
) derives Order:
  lazy val location: Offset = (offsetPos - pivot).rotate(posAngle) + pivot

sealed trait AgsGeomCalc:
  // Indicates if the given offset is reachable
  def isReachable(gsOffset: Offset): Boolean

  // Calculates the area vignetted at a given offset
  def vignettingArea(gsOffset: Offset): Area

  // Indicates if the given guide star would vignette the science target
  def overlapsScience(gsOffset: Offset): Boolean

  def intersectionPatrolField: ShapeExpression

trait SingleProbeAgsParams:
  def patrolFieldAt(posAngle: Angle, offset: Offset, pivot: Offset = Offset.Zero): ShapeExpression

  def scienceArea(posAngle: Angle, offset: Offset): ShapeExpression

  def probeArm(posAngle: Angle, guideStar: Offset, offset: Offset): ShapeExpression

  def scienceRadius: Angle

  def posCalculations(
    positions: NonEmptyList[AgsPosition]
  ): NonEmptyMap[AgsPosition, AgsGeomCalc] =
    val result = positions.map: position =>
      position -> new AgsGeomCalc() {
        override val intersectionPatrolField: ShapeExpression =
          positions
            .map(pos => (pos.offsetPos, pos.pivot))
            .distinct
            .map((offset, pivot) => patrolFieldAt(position.posAngle, offset, pivot))
            .reduce(using _ ∩ _)

        private val scienceAreaShape =
          scienceArea(position.posAngle, position.offsetPos)

        private val scienceTargetArea =
          ShapeExpression.centeredEllipse(scienceRadius,
                                          scienceRadius
          ) ↗ position.offsetPos ⟲ position.posAngle

        // Cache evaluated shapes to avoid re-computation on each call
        private val intersectionShape: Shape =
          intersectionPatrolField.eval

        private val scienceTargetShape: Shape =
          scienceTargetArea.eval

        private val scienceAreaShapeEval: Shape =
          scienceAreaShape.eval

        override def isReachable(gsOffset: Offset): Boolean =
          intersectionShape.contains(gsOffset)

        def overlapsScience(gsOffset: Offset): Boolean =
          probeArm(position.posAngle, gsOffset, position.offsetPos).eval
            .intersects(scienceTargetShape)

        override def vignettingArea(gsOffset: Offset): Area =
          probeArm(position.posAngle, gsOffset, position.offsetPos).eval
            .intersection(scienceAreaShapeEval).area

      }
    result.toNem

sealed trait AgsParams derives Eq:

  def probe: GuideProbe

  // Builds an AgsGeom object for each position
  // The geometries won't chage with the position and we can cache them
  def posCalculations(
    positions: NonEmptyList[AgsPosition]
  ): NonEmptyMap[AgsPosition, AgsGeomCalc]

object AgsParams:
  case class GmosAgsParams(
    fpu:  Option[Either[GmosNorthFpu, GmosSouthFpu]],
    port: PortDisposition
  ) extends AgsParams
      with SingleProbeAgsParams derives Eq:
    import lucuma.core.geom.{gmos => GmosGeom}

    val GmosScienceRadius = 20.arcseconds

    override val probe = GuideProbe.GmosOIWFS

    override def patrolFieldAt(
      posAngle: Angle,
      offset:   Offset,
      pivot:    Offset = Offset.Zero
    ): ShapeExpression =
      GmosGeom.patrolField.patrolFieldAt(posAngle, offset, fpu, port, pivot)

    override def scienceArea(posAngle: Angle, offset: Offset): ShapeExpression =
      GmosGeom.scienceArea.shapeAt(posAngle, offset, fpu)

    override def probeArm(posAngle: Angle, guideStar: Offset, offset: Offset): ShapeExpression =
      GmosGeom.probeArm.shapeAt(posAngle, guideStar, offset, fpu, port)

    override def scienceRadius: Angle = GmosScienceRadius

  case class Flamingos2AgsParams(
    lyot: Flamingos2LyotWheel,
    fpu:  Flamingos2FpuMask,
    port: PortDisposition
  ) extends AgsParams
      with SingleProbeAgsParams derives Eq:
    import lucuma.core.geom.{flamingos2 => Flamingos2Geom}

    val Flamingos2ScienceRadius = 20.arcseconds

    override val probe = GuideProbe.Flamingos2OIWFS

    override def patrolFieldAt(
      posAngle: Angle,
      offset:   Offset,
      pivot:    Offset = Offset.Zero
    ): ShapeExpression =
      Flamingos2Geom.patrolField.patrolFieldAt(posAngle, offset, lyot, port, pivot)

    override def scienceArea(posAngle: Angle, offset: Offset): ShapeExpression =
      Flamingos2Geom.scienceArea.shapeAt(posAngle, offset, lyot, fpu)

    override def probeArm(posAngle: Angle, guideStar: Offset, offset: Offset): ShapeExpression =
      Flamingos2Geom.probeArm.shapeAt(posAngle, guideStar, offset, lyot, port)

    override def scienceRadius: Angle = Flamingos2ScienceRadius

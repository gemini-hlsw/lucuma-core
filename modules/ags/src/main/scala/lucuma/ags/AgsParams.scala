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
import lucuma.core.geom.BoundingOffsets
import lucuma.core.geom.Shape
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.offsets.OffsetPosition
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask

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
    positions: NonEmptyList[OffsetPosition]
  ): NonEmptyMap[OffsetPosition, AgsGeomCalc] =
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

        // Cache shapes to avoid re-computation
        private val intersectionShape: Shape =
          intersectionPatrolField.eval

        // Cache bounding box for fast rejection
        private val intersectionBounds: BoundingOffsets =
          intersectionShape.boundingOffsets

        private val scienceTargetShape: Shape =
          scienceTargetArea.eval

        private val scienceAreaShapeEval: Shape =
          scienceAreaShape.eval

        override def isReachable(gsOffset: Offset): Boolean =
          // Fast bounding box rejection, then precise check
          intersectionBounds.contains(gsOffset) && intersectionShape.contains(gsOffset)

        def overlapsScience(gsOffset: Offset): Boolean =
          probeArm(position.posAngle, gsOffset, position.offsetPos).eval
            .intersects(scienceTargetShape)

        override def vignettingArea(gsOffset: Offset): Area =
          probeArm(position.posAngle, gsOffset, position.offsetPos).eval
            .intersection(scienceAreaShapeEval)
            .area

      }
    result.toNem

trait PwfsSupport[A]:
  def probe: GuideProbe
  def withPWFS1: A = withProbe(GuideProbe.PWFS1)
  def withPWFS2: A = withProbe(GuideProbe.PWFS2)
  def withProbe(probe: GuideProbe): A

sealed trait AgsParams derives Eq:

  def probe: GuideProbe

  // Builds an AgsGeom object for each position
  // The geometries won't chage with the position and we can cache them
  def posCalculations(
    positions: NonEmptyList[OffsetPosition]
  ): NonEmptyMap[OffsetPosition, AgsGeomCalc]

object AgsParams:
  case class GmosAgsParams(
    fpu:                Option[Either[GmosNorthFpu, GmosSouthFpu]],
    port:               PortDisposition,
    override val probe: GuideProbe
  ) extends AgsParams
      with SingleProbeAgsParams
      with PwfsSupport[GmosAgsParams] derives Eq:
    import lucuma.core.geom.gmos
    import lucuma.core.geom.gmos.oiwfs
    import lucuma.core.geom.pwfs

    def withProbe(probe: GuideProbe): GmosAgsParams = copy(probe = probe)

    override def patrolFieldAt(
      posAngle: Angle,
      offset:   Offset,
      pivot:    Offset = Offset.Zero
    ): ShapeExpression =
      probe match
        case GuideProbe.GmosOIWFS                =>
          oiwfs.patrolField.patrolFieldAt(posAngle, offset, fpu, port, pivot)
        case GuideProbe.PWFS1 | GuideProbe.PWFS2 =>
          pwfs.patrolField.patrolFieldAt(posAngle, offset, pivot)
        case _                                   =>
          ShapeExpression.empty

    override def scienceArea(posAngle: Angle, offset: Offset): ShapeExpression =
      gmos.scienceArea.shapeAt(posAngle, offset, fpu)

    override def probeArm(posAngle: Angle, guideStar: Offset, offset: Offset): ShapeExpression =
      probe match
        case GuideProbe.GmosOIWFS                =>
          oiwfs.probeArm.shapeAt(posAngle, guideStar, offset, fpu, port)
        case GuideProbe.PWFS1 | GuideProbe.PWFS2 =>
          pwfs.probeArm.vignettedAreaAt(probe, guideStar, offset)
        case _                                   =>
          ShapeExpression.Empty

    override def scienceRadius: Angle = GmosAgsParams.GmosScienceRadius

  object GmosAgsParams:
    def apply(
      fpu:  Option[Either[GmosNorthFpu, GmosSouthFpu]],
      port: PortDisposition
    ): GmosAgsParams = GmosAgsParams(fpu, port, GuideProbe.GmosOIWFS)

    val GmosScienceRadius = 20.arcseconds

  case class Flamingos2AgsParams private (
    lyot:               Flamingos2LyotWheel,
    fpu:                Flamingos2FpuMask,
    port:               PortDisposition,
    override val probe: GuideProbe
  ) extends AgsParams
      with SingleProbeAgsParams
      with PwfsSupport[Flamingos2AgsParams] derives Eq:
    import lucuma.core.geom.flamingos2
    import lucuma.core.geom.flamingos2.oiwfs
    import lucuma.core.geom.pwfs

    def withProbe(probe: GuideProbe): Flamingos2AgsParams = copy(probe = probe)

    override def patrolFieldAt(
      posAngle: Angle,
      offset:   Offset,
      pivot:    Offset = Offset.Zero
    ): ShapeExpression =
      probe match
        case GuideProbe.Flamingos2OIWFS          =>
          oiwfs.patrolField.patrolFieldAt(posAngle, offset, lyot, port, pivot)
        case GuideProbe.PWFS1 | GuideProbe.PWFS2 =>
          pwfs.patrolField.patrolFieldAt(posAngle, offset, pivot)
        case _                                   => ShapeExpression.Empty

    override def scienceArea(posAngle: Angle, offset: Offset): ShapeExpression =
      flamingos2.scienceArea.shapeAt(posAngle, offset, lyot, fpu)

    override def probeArm(posAngle: Angle, guideStar: Offset, offset: Offset): ShapeExpression =
      probe match
        case GuideProbe.Flamingos2OIWFS          =>
          oiwfs.probeArm.shapeAt(posAngle, guideStar, offset, lyot, port)
        case GuideProbe.PWFS1 | GuideProbe.PWFS2 =>
          pwfs.probeArm.vignettedAreaAt(probe, guideStar, offset)
        case _                                   => ShapeExpression.Empty

    override def scienceRadius: Angle = Flamingos2AgsParams.Flamingos2ScienceRadius

  object Flamingos2AgsParams:
    def apply(
      lyot: Flamingos2LyotWheel,
      fpu:  Flamingos2FpuMask,
      port: PortDisposition
    ): Flamingos2AgsParams = Flamingos2AgsParams(lyot, fpu, port, GuideProbe.Flamingos2OIWFS)

    val Flamingos2ScienceRadius = 20.arcseconds

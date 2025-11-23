// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.jts
package demo

import cats.data.NonEmptyList
import cats.syntax.option.*
import lucuma.ags.AcquisitionOffsets
import lucuma.ags.Ags
import lucuma.ags.AgsParams.GmosAgsParams
import lucuma.ags.AgsPosition
import lucuma.ags.AgsVisualization
import lucuma.ags.GeometryType
import lucuma.ags.ScienceOffsets
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.PortDisposition
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*

import java.awt.{List as _, *}

trait GmosAgsVisualizationShapes extends InstrumentShapes:
  import lucuma.core.geom.gmos.*

  val posAngle: Angle = 145.deg

  val guideStarOffset: Offset =
    Offset(170543999.µas.p, -24177003.µas.q)

  val fpu: Option[Either[GmosNorthFpu, GmosSouthFpu]] =
    Some(Right(GmosSouthFpu.LongSlit_5_00))

  val port: PortDisposition = PortDisposition.Side

  val baseCoordinates: Coordinates = Coordinates.Zero

  // Blind offset as an offset
  val blindOffsetRaw: Offset = Offset(-60.arcsec.p, 270.arcsec.q)
  // We need coordinates for ags
  val blindOffset: Coordinates =
    baseCoordinates.offsetBy(Angle.Angle0, blindOffsetRaw).get

  // Acquisition offsets (small dither pattern)
  val acqOffsets: AcquisitionOffsets = AcquisitionOffsets(
    NonEmptyList.of(
      Offset.Zero,
      Offset(10.arcsec.p, 0.arcsec.q)
    )
  )

  val scienceOffsets: ScienceOffsets = ScienceOffsets(
    NonEmptyList.of(
      Offset(0.arcsec.p, 15.arcsec.q),
      Offset.Zero,
      Offset(0.arcsec.p, -15.arcsec.q)
    )
  )

  // each relevant ags position
  val positions: NonEmptyList[(GeometryType, AgsPosition)] =
    Ags.generatePositionGeometries(
      baseCoordinates,
      blindOffset.some,
      NonEmptyList.one(posAngle),
      acqOffsets.some,
      scienceOffsets.some
    )

  val params: GmosAgsParams = GmosAgsParams(fpu, port)

  val patrolViz = AgsVisualization.patrolFieldGeometries(params, positions)
  pprint.pprintln(patrolViz.toList.map(_.geometryType))

  val scienceViz = AgsVisualization.scienceOverlapVisualization(
    params,
    AgsPosition(posAngle, Offset.Zero),
    guideStarOffset
  )

  val shapes: List[ShapeExpression] = List(
    scienceViz.probeArmShape,
    scienceViz.scienceAreaShape,
    candidatesArea.candidatesAreaAt(posAngle, Offset.Zero)
  )

  private def crossAt(offset: Offset): ShapeExpression = {
    val size = 7.arcsec
    val width = 500.mas
    val h = ShapeExpression.centeredRectangle(size, width)
    val v = ShapeExpression.centeredRectangle(width, size)
    (h ∪ v) ↗ offset ⟲ posAngle
  }

  val solidStroke = new BasicStroke(1.5f)
  val tinyStroke = new BasicStroke(4f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10f, Array(1f, 1f), 0f)
  val smallDashStroke = new BasicStroke(1f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10f, Array(3f, 3f), 0f)
  val largeDashStroke = new BasicStroke(1f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10f, Array(8f, 4f), 0f)
  val thickStroke = new BasicStroke(2.5f)

  // Color and stroke mapping by GeometryType
  val geometryStyle: Map[GeometryType, (Color, BasicStroke)] = Map(
    GeometryType.BlindOffset  -> (Color.RED, thickStroke),
    GeometryType.AcqOffset    -> (Color.ORANGE, largeDashStroke),
    GeometryType.SciOffset    -> (Color.BLUE, smallDashStroke),
    GeometryType.Base         -> (Color.MAGENTA, solidStroke),
    GeometryType.Intersection -> (Color.CYAN, solidStroke)
  )

  override val coloredShapes: List[ColoredShape] = {
    val patrolFields = patrolViz.sortBy(_.geometryType).toList.map { v =>
      val (color, stroke) = geometryStyle.getOrElse(v.geometryType, (Color.GRAY, solidStroke))
      ColoredShape(v.posPatrolField, color, Some(stroke), Some(v.geometryType))
    }

    val intersection = ColoredShape(
      patrolViz.head.paIntersection,
      Color.YELLOW,
      Some(tinyStroke)
    )

    val scienceTarget = ColoredShape(
      scienceViz.scienceTargetAreaShape,
      Color.YELLOW
    )

    val overlap = if (scienceViz.overlapsScience)
      List(ColoredShape(scienceViz.targetOverlap, Color.RED))
    else
      Nil

    val vignetting = ColoredShape(
      scienceViz.detectorVignetting,
      new Color(255, 0, 0, 100)
    )

    val scienceOffPos = scienceOffsets.value.toList.map(offset =>
      ColoredShape(crossAt(offset), Color.BLUE)
    )

    val acqOffPos = acqOffsets.value.toList.map(offset =>
      ColoredShape(crossAt(offset + blindOffsetRaw), Color.ORANGE)
    )

    val blindMarker = ColoredShape(
      ShapeExpression.centeredEllipse(25.arcsec, 25.arcsec) ↗ blindOffsetRaw ⟲ posAngle,
      Color.RED
    )

    patrolFields  ++ List(intersection, scienceTarget, vignetting, blindMarker) ++ overlap ++ scienceOffPos ++ acqOffPos
  }

object JtsGmosAgsVisualizationDemo extends JtsDemo with GmosAgsVisualizationShapes

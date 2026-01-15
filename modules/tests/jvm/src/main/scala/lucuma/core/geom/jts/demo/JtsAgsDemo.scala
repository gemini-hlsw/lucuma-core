// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.jts
package demo

import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.syntax.eq.*
import cats.syntax.option.*
import lucuma.ags.AcquisitionOffsets
import lucuma.ags.Ags
import lucuma.ags.AgsParams.GmosAgsParams
import lucuma.ags.AgsVisualization
import lucuma.ags.ScienceOffsets
import lucuma.ags.syntax.*
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.PortDisposition
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.offsets.GeometryType
import lucuma.core.geom.offsets.OffsetPosition
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.math.syntax.int.*

import java.awt.{List as _, *}

trait GmosAgsVisualizationShapes(val posAngle: Angle) extends InstrumentShapes:
  import lucuma.core.geom.gmos.*

  val guideStarOffset: Offset =
    Offset(170543999.µas.p, -24177003.µas.q)

  val fpu: Option[Either[GmosNorthFpu, GmosSouthFpu]] =
    Some(Right(GmosSouthFpu.LongSlit_5_00))

  val port: PortDisposition = PortDisposition.Side

  val baseCoordinates: Coordinates = Coordinates.Zero

  // Blind offset as an offset
  val blindOffsetRaw: Offset = Offset(0.arcsec.p, 70.arcsec.q)
  // We need coordinates for ags
  val blindOffset: Coordinates =
    baseCoordinates.offsetBy(Angle.Angle0, blindOffsetRaw).get

  val acqOffsets: AcquisitionOffsets = AcquisitionOffsets(
    NonEmptySet.of(
      Offset(0.arcsec.p, -10.arcsec.q).guided,
      Offset(10.arcsec.p,  0.arcsec.q).guided
    )
  )

  val scienceOffsets: ScienceOffsets = ScienceOffsets(
    NonEmptySet.of(
      Offset(0.arcsec.p,  15.arcsec.q).guided,
      Offset.Zero.guided,
      Offset(0.arcsec.p, -15.arcsec.q).guided
    )
  )

  // each relevant ags position
  lazy val positions: NonEmptyList[OffsetPosition] =
    Ags.generatePositions(
      baseCoordinates.some,
      blindOffset.some,
      NonEmptyList.one(posAngle),
      acqOffsets.some,
      scienceOffsets.some
    ).value.toNonEmptyList

  val params: GmosAgsParams = GmosAgsParams(fpu, port)

  lazy val patrolViz = AgsVisualization.patrolFieldGeometries(params, positions)

  lazy val scienceViz = AgsVisualization.scienceOverlapVisualization(
    params,
    OffsetPosition(GeometryType.Base, Offset.Zero, posAngle),
    guideStarOffset
  )

  override def shapes: List[ShapeExpression] = List(
    scienceViz.probeArmShape,
    scienceViz.scienceAreaShape,
    candidatesArea.candidatesAreaAt(posAngle, Offset.Zero)
  )

  private def crossAt(offset: Offset): ShapeExpression = {
    val size = 7.arcsec
    val width = 500.mas
    val h = ShapeExpression.centeredRectangle(size, width)
    val v = ShapeExpression.centeredRectangle(width, size)
    (h ∪ v) ↗ offset
  }

  val solidStroke = new BasicStroke(1.5f)
  val tinyStroke = new BasicStroke(4f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10f, Array(1f, 1f), 0f)
  val smallDashStroke = new BasicStroke(1f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10f, Array(3f, 3f), 0f)
  val largeDashStroke = new BasicStroke(1f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10f, Array(8f, 4f), 0f)
  val thickStroke = new BasicStroke(2.5f)

  // Color and stroke mapping by GeometryType
  val geometryStyle: Map[GeometryType, (Color, BasicStroke)] = Map(
    GeometryType.BlindOffset     -> (Color.RED, thickStroke),
    GeometryType.AcqGuidedOffset -> (Color.CYAN, largeDashStroke),
    GeometryType.SciGuidedOffset -> (Color.BLUE, smallDashStroke),
    GeometryType.Base            -> (Color.MAGENTA, solidStroke),
    GeometryType.AgsIntersection -> (Color.BLACK, solidStroke)
  )

  override def coloredShapes: List[ColoredShape] = {

    val excludedGeometries = Nil
      //List(GeometryType.AcqOffset, GeometryType.Base)

    val patrolFields =
      patrolViz
        .sortBy(_.position.geometryType)
        // Sometimes is useful to only show some geometry types
        .filterNot(v => excludedGeometries.exists(v.position.geometryType === _))
        .toList
        .map: v =>
          val (color, stroke) = geometryStyle.getOrElse(v.position.geometryType, (Color.GRAY, solidStroke))
          ColoredShape(v.posPatrolField, color, stroke.some, v.position.geometryType.some)

    val intersection = ColoredShape(
      patrolViz.head.paIntersection,
      Color.GREEN,
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

    val scienceOffPos = scienceOffsets.value.toSortedSet.toList.map: offset =>
      ColoredShape(crossAt(offset.value.rotate(posAngle)), Color.BLUE)

    // Acquisition offset markers - use the actual location from positions
    val acqOffPos = positions.toList
      .filter(_.geometryType == GeometryType.AcqGuidedOffset)
      .map: pos =>
        ColoredShape(crossAt(pos.rotatedOffset.value), Color.ORANGE)

    val blindMarker = ColoredShape(
      ShapeExpression.centeredEllipse(25.arcsec, 25.arcsec) ↗ blindOffsetRaw,
      Color.RED
    )

    patrolFields  ++ List(intersection, scienceTarget, vignetting, blindMarker) ++ overlap ++ scienceOffPos ++ acqOffPos
  }

object JtsGmosAgsVisualizationDemo:

  def demo(posAngle: Angle) =
    new JtsDemo with GmosAgsVisualizationShapes(posAngle)

  def main(args: Array[String]): Unit =
    val posAngle = args
      .collectFirst {
        case s if s.startsWith("--posAngle=") =>
          val value = s.stripPrefix("--posAngle=").toDouble
          Angle.fromDoubleDegrees(value)
      }
      .getOrElse(145.deg)

    demo(posAngle).main(args)

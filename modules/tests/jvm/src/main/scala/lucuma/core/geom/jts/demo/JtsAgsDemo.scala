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
import lucuma.ags.AgsParams.GmosLongSlit
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
import lucuma.core.model.PosAngleConstraint

import java.awt.{List as _, *}

trait AgsVisualizationBase extends InstrumentShapes:
  val solidStroke     = new BasicStroke(1.5f)
  val thinStroke      = new BasicStroke(0.5f)
  val dashStroke      = new BasicStroke(1f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10f, Array(4f, 4f), 0f)
  val tinyStroke      = new BasicStroke(4f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10f, Array(1f, 1f), 0f)
  val smallDashStroke = new BasicStroke(1f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10f, Array(3f, 3f), 0f)
  val largeDashStroke = new BasicStroke(1f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10f, Array(8f, 4f), 0f)
  val thickStroke     = new BasicStroke(2.5f)

object AgsVisualizationDemo:
  def parsePosAngle(args: Array[String], default: Angle): Angle =
    args
      .collectFirst {
        case s if s.startsWith("--posAngle=") =>
          Angle.fromDoubleDegrees(s.stripPrefix("--posAngle=").toDouble)
      }
      .getOrElse(default)

trait GmosAgsVisualizationShapes(val posAngle: Angle) extends AgsVisualizationBase:
  import lucuma.core.geom.gmos.*

  val guideStarOffset: Offset =
    Offset(-170543999.µas.p, -24177003.µas.q)

  val defaultFpu: Either[GmosNorthFpu, GmosSouthFpu] =
    Right(GmosSouthFpu.LongSlit_5_00)

  val anglesToTest: NonEmptyList[Angle] =
    PosAngleConstraint.Unbounded.anglesToTestAt(None).get

  val fpu: Either[GmosNorthFpu, GmosSouthFpu] = defaultFpu

  val port: PortDisposition = PortDisposition.Side

  val baseCoordinates: Coordinates = Coordinates.Zero

  val blindOffsetRaw: Offset = Offset(0.arcsec.p, 70.arcsec.q)
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
      anglesToTest,
      acqOffsets.some,
      scienceOffsets.some
    ).value.toNonEmptyList

  val params: GmosLongSlit = GmosLongSlit(fpu, port)

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

  private def crossAt(offset: Offset): ShapeExpression =
    val size = 7.arcsec
    val width = 500.mas
    val h = ShapeExpression.centeredRectangle(size, width)
    val v = ShapeExpression.centeredRectangle(width, size)
    (h ∪ v) ↗ offset

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
    demo(AgsVisualizationDemo.parsePosAngle(args, 145.deg)).main(args)

trait GmosWithPwfsVisualizationShapes(val posAngle: Angle) extends AgsVisualizationBase:
  import lucuma.core.geom.pwfs.probeArm

  val guideStarOffset: Offset =
    Offset(200.arcsec.p, -150.arcsec.q)

  val defaultFpu: Option[Either[GmosNorthFpu, GmosSouthFpu]] =
    Some(Right(GmosSouthFpu.LongSlit_5_00))

  val anglesToTest: NonEmptyList[Angle] =
    // This coulb be replaced by UconstraindAngles but lets us test more easily
    PosAngleConstraint.Unbounded.anglesToTestAt(None).get

  val offsetPos: Offset = Offset.Zero

  val params = GmosLongSlit(defaultFpu.get, PortDisposition.Side).withPWFS2

  override def shapes: List[ShapeExpression] = List(
    params.scienceArea(posAngle, offsetPos),
  )

  override def coloredShapes: List[ColoredShape] =
    val guideStarMarker = ColoredShape(
      ShapeExpression.centeredRectangle(2.arcsec, 2.arcsec) ↗ guideStarOffset,
      Color.GREEN,
      solidStroke.some
    )

    val colors = List(Color.ORANGE, Color.CYAN, Color.MAGENTA, Color.YELLOW, Color.PINK, Color.LIGHT_GRAY)
    val patrolFields = anglesToTest.toList.zipWithIndex.map { case (angle, idx) =>
      ColoredShape(
        params.patrolFieldAt(angle, offsetPos),
        colors(idx % colors.length),
        dashStroke.some
      )
    }

    val intersection = ColoredShape(
      anglesToTest.toList.map(params.patrolFieldAt(_, offsetPos)).reduce(_ ∩ _),
      Color.GREEN,
      solidStroke.some
    )

    val mirror = ColoredShape(
      probeArm.mirrorAt(params.probe, guideStarOffset, offsetPos),
      Color.RED,
      solidStroke.some
    )
    val mirrorVignetted = ColoredShape(
      probeArm.mirrorVignettedAreaAt(params.probe, guideStarOffset, offsetPos),
      Color.BLUE,
      thinStroke.some
    )
    val armVignetted = ColoredShape(
      probeArm.armVignettedAreaAt(params.probe, guideStarOffset, offsetPos),
      Color.BLUE,
      thinStroke.some
    )
    val arm = ColoredShape(
      probeArm.armAt(params.probe, guideStarOffset, offsetPos),
      Color.DARK_GRAY,
      thinStroke.some
    )
    patrolFields ++ List(intersection, mirrorVignetted, armVignetted, arm, mirror, guideStarMarker)

object JtsGmosAgsPwfsDemo:
  def demo(posAngle: Angle) =
    new JtsDemo with GmosWithPwfsVisualizationShapes(posAngle):
      override val arcsecPerPixel: Double = 0.5

  def main(args: Array[String]): Unit =
    demo(AgsVisualizationDemo.parsePosAngle(args, 0.deg)).main(args)

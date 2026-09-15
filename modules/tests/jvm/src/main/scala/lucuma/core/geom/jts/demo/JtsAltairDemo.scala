// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.jts
package demo

import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.syntax.option.*
import lucuma.ags.Ags
import lucuma.ags.AgsParams.GnirsLongSlit
import lucuma.ags.AgsVisualization
import lucuma.ags.ScienceOffsets
import lucuma.ags.syntax.*
import lucuma.core.enums.AltairMode
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GuideProbe
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

/**
 * GNIRS long slit behind Altair NGS: the AOWFS oval at every AGS position (the requested position
 * angle and its flip, each nodded along the slit), their intersection, the candidates circle and
 * the science area. The oval is taller towards positive q, which the flipped angle makes visible.
 */
trait GnirsAltairVisualizationShapes(val posAngle: Angle) extends AgsVisualizationBase:

  val guideStarOffset: Offset = Offset(12.arcsec.p, 18.arcsec.q)

  val anglesToTest: NonEmptyList[Angle] =
    NonEmptyList.of(posAngle, posAngle + Angle.Angle180)

  // ABBA-style nods along the slit
  val scienceOffsets: ScienceOffsets =
    ScienceOffsets(
      NonEmptySet.of(
        Offset(0.arcsec.p, 5.arcsec.q).guided,
        Offset.Zero.guided,
        Offset(0.arcsec.p, -(5.arcsec.q)).guided
      )
    )

  lazy val positions: NonEmptyList[OffsetPosition] =
    Ags
      .generatePositions(Coordinates.Zero.some, None, anglesToTest, None, scienceOffsets.some)
      .value
      .toNonEmptyList

  val params: GnirsLongSlit =
    GnirsLongSlit(
      GnirsFpuSlit.LongSlit_0_30,
      GnirsCamera.LongBlue,
      GnirsPrism.Mirror,
      PortDisposition.Bottom
    ).withAltair(AltairMode.Ngs)

  lazy val patrolViz = AgsVisualization.patrolFieldGeometries(params, positions)

  override def shapes: List[ShapeExpression] =
    List(
      params.scienceArea(posAngle, Offset.Zero),
      GuideProbe.AltairAOWFS.candidatesArea
    )

  val geometryStyle: Map[GeometryType, (Color, BasicStroke)] = Map(
    GeometryType.SciGuidedOffset -> (Color.BLUE, smallDashStroke),
    GeometryType.Base            -> (Color.MAGENTA, solidStroke),
    GeometryType.AgsIntersection -> (Color.BLACK, solidStroke)
  )

  override def coloredShapes: List[ColoredShape] =
    val patrolFields: List[ColoredShape] =
      patrolViz.toList.map: v =>
        val (color, stroke) =
          geometryStyle.getOrElse(v.position.geometryType, (Color.GRAY, solidStroke))
        ColoredShape(v.shape, color, stroke.some, v.position.geometryType.some)
    val intersection: ColoredShape           =
      ColoredShape(patrolViz.head.paIntersection, Color.GREEN, tinyStroke.some)
    val guideStar: ColoredShape              =
      ColoredShape(
        ShapeExpression.centeredRectangle(1.arcsec, 1.arcsec) ↗ guideStarOffset,
        Color.RED,
        solidStroke.some
      )
    patrolFields ++ List(intersection, guideStar)

object JtsGnirsAltairDemo:
  def demo(posAngle: Angle) =
    new JtsDemo with GnirsAltairVisualizationShapes(posAngle):
      override val arcsecPerPixel: Double = 0.1
      override val gridSize: Angle        = 10.arcsec

  def main(args: Array[String]): Unit =
    demo(AgsVisualizationDemo.parsePosAngle(args, 0.deg)).main(args)

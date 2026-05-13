// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.gnirs

import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFpuOther
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsPixelScale
import lucuma.core.enums.GnirsPrism
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset

/**
 * GNIRS science area geometry
 */
trait GnirsScienceAreaGeometry:

  def base: ShapeExpression =
    ShapeExpression.point(Offset.Zero)

  private def slitLength(camera: GnirsCamera, prism: GnirsPrism): Angle =
    (camera.pixelScale, prism) match
      case (GnirsPixelScale.PixelScale_0_05, GnirsPrism.Mirror) => SlitLengthLongCamNoXd
      case (GnirsPixelScale.PixelScale_0_05, GnirsPrism.Sxd)    => SlitLengthLongCamSxd
      case (GnirsPixelScale.PixelScale_0_05, GnirsPrism.Lxd)    => SlitLengthLongCamLxd
      case (GnirsPixelScale.PixelScale_0_15, GnirsPrism.Mirror) => SlitLengthShortCamNoXd
      case (GnirsPixelScale.PixelScale_0_15, _)                 => SlitLengthShortCamXd

  private def longSlitFov(
    slit:   GnirsFpuSlit,
    camera: GnirsCamera,
    prism:  GnirsPrism
  ): ShapeExpression =
    ShapeExpression.centeredRectangle(slit.slitWidth, slitLength(camera, prism))

  private def pinholeFov(other: GnirsFpuOther): Option[ShapeExpression] =
    other match
      case GnirsFpuOther.Pinhole1                                =>
        Some(ShapeExpression.centeredEllipse(Pinhole1Size, Pinhole1Size))
      case GnirsFpuOther.Pinhole3                                =>
        Some(ShapeExpression.centeredEllipse(Pinhole3Size, Pinhole3Size))
      case GnirsFpuOther.Acquisition | GnirsFpuOther.PupilViewer =>
        None

  def shapeAt(
    posAngle:  Angle,
    offsetPos: Offset,
    fpu:       Either[GnirsFpuSlit, GnirsFpuOther],
    camera:    GnirsCamera,
    prism:     GnirsPrism
  ): Option[ShapeExpression] =
    fpu
      .fold(slit => Some(longSlitFov(slit, camera, prism)), pinholeFov)
      .map(_.shapeAt(offsetPos, posAngle))

object scienceArea extends GnirsScienceAreaGeometry

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.geom.gnirs

import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuIfu
import lucuma.core.enums.GnirsFpuOther
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsPixelScale
import lucuma.core.enums.GnirsPrism
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.sequence.gnirs.GnirsFpu

/**
 * GNIRS science area geometry
 */
trait GnirsScienceAreaGeometry:

  def base: ShapeExpression =
    ShapeExpression.point(Offset.Zero)

  def slitLength(camera: GnirsCamera, prism: GnirsPrism): Angle =
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

  // Y-MK, J-MK and K-MK filters see the smaller, round unvignetted field; every other
  // imaging filter (order-blocking, narrow-band, H-MK) sees the full keyhole.
  def isRoundImagingFilter(filter: GnirsFilter): Boolean =
    filter match
      case GnirsFilter.Y | GnirsFilter.J | GnirsFilter.K => true
      case _                                             => false

  private def asec(a: Angle): Double = Angle.signedDecimalArcseconds.get(a).toDouble
  private def arcsec(d: Double): Angle = Angle.fromDoubleArcseconds(d)
  private def offP(pArcsec: Double): Offset =
    Offset.signedMicroarcseconds.reverseGet(((pArcsec * 1e6).round, 0L))

  // A circular cap (segment) protruding toward +p from the +p edge of a bar of the
  // given p-width (centered on the origin). `chord` is the cap's extent along q, and
  // `sagitta` its protrusion along p; the radius is implied by the two. The long field
  // axis runs along q (like the long slit), so the cap bumps out sideways.
  // See https://www.gemini.edu/sciops/instruments/nirs/filters/imaging_aps.jpg
  private def cap(chord: Angle, sagitta: Angle, barWidth: Angle): ShapeExpression =
    val c       = asec(chord)
    val s       = asec(sagitta)
    val baseP   = asec(barWidth) / 2                   // cap springs from the +p edge of the bar
    val r       = (s * s + (c / 2) * (c / 2)) / (2 * s) // circle radius from chord & sagitta
    val circle  = ShapeExpression.centeredEllipse(arcsec(2 * r), arcsec(2 * r)) ↗ offP(baseP - (r - s))
    val clip    = ShapeExpression.centeredRectangle(arcsec(2 * s + 2), arcsec(c + 2)) ↗ offP(baseP + s + 1)
    circle ∩ clip

  // GNIRS imaging science area ("keyhole"): the 99"/49" no-XD field along q (camera-
  // dependent, like the long slit) as a narrow bar, with a circular cap bumping out to
  // one side; or, for the MK filters, the smaller round field. Widths below are the
  // diagram's horizontal (mapped to q); heights are its vertical (the spatial cross-axis, p).
  private def imagingFov(camera: GnirsCamera, filter: GnirsFilter): ShapeExpression =
    if isRoundImagingFilter(filter) then
      ShapeExpression.centeredRectangle(RoundFieldHeight, RoundFieldWidth) ∪
        cap(RoundCapWidth, RoundCapHeight, RoundFieldHeight)
    else
      ShapeExpression.centeredRectangle(KeyholeBarHeight, slitLength(camera, GnirsPrism.Mirror)) ∪
        cap(KeyholeCapWidth, KeyholeCapHeight, KeyholeBarHeight)

  // IFU science area: a rectangle of the IFU "slit width" by a fixed,
  // resolution-dependent height (derived from ocs InstGNIRS.getScienceArea).
  private def ifuFov(ifu: GnirsFpuIfu): ShapeExpression =
    val height = ifu match
      case GnirsFpuIfu.LowResolution  => IfuLowResHeight
      case GnirsFpuIfu.HighResolution => IfuHighResHeight
    ShapeExpression.centeredRectangle(ifu.slitWidth, height)

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
    fpu:       GnirsFpu,
    camera:    GnirsCamera,
    prism:     GnirsPrism
  ): Option[ShapeExpression] =
    fpu
      .fold(
        slit => Some(longSlitFov(slit, camera, prism)),
        ifu => Some(ifuFov(ifu)),
        pinholeFov
      )
      .map(_.shapeAt(offsetPos, posAngle))

  def longSlitShapeAt(
    posAngle:  Angle,
    offsetPos: Offset,
    slit:      GnirsFpuSlit,
    camera:    GnirsCamera,
    prism:     GnirsPrism
  ): ShapeExpression =
    longSlitFov(slit, camera, prism).shapeAt(offsetPos, posAngle)

  def imagingShapeAt(
    posAngle:  Angle,
    offsetPos: Offset,
    camera:    GnirsCamera,
    filter:    GnirsFilter
  ): ShapeExpression =
    imagingFov(camera, filter).shapeAt(offsetPos, posAngle)

  def ifuShapeAt(
    posAngle:  Angle,
    offsetPos: Offset,
    ifu:       GnirsFpuIfu
  ): ShapeExpression =
    ifuFov(ifu).shapeAt(offsetPos, posAngle)

object scienceArea extends GnirsScienceAreaGeometry

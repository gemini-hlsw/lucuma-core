// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.mos

import cats.Eq
import cats.Show
import cats.derived.*
import cats.syntax.eq.*
import lucuma.core.enums.MosSlitPriority
import lucuma.core.enums.MosSlitType
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Coordinates
import lucuma.core.math.Redshift
import lucuma.core.refined.cats.given
import monocle.Focus
import monocle.Lens

/**
 * One aperture cut in a MOS mask, together with the object it targets.
 *
 * Slit dimensions are expressed physically rather than as the file's x and y columns: `slitWidth`
 * is always the extent along the dispersion direction and `slitLength` always the extent
 * perpendicular to it, regardless of instrument. A parser is responsible for resolving the file's
 * axis convention, so a decoded slit needs no further context to be understood.
 *
 * @param id               object identifier, unique within the mask
 * @param coordinates      sky position of the object; used for display, not for slit placement
 * @param x                object position on the pre-image, in pixels
 * @param y                object position on the pre-image, in pixels
 * @param magnitude        relative magnitude of the object
 * @param slitWidth        extent along the dispersion direction; sets spectral resolution
 * @param slitLength       extent perpendicular to the dispersion direction
 * @param offsetAlongSlit  displacement of the slit from the object, along the slit's length
 * @param offsetAcrossSlit displacement of the slit from the object, across its width; a non-zero
 *                         value drives a point source off the slit and loses flux
 * @param tilt             slit position angle, counter-clockwise positive; bounded to 45 degrees
 * @param redshift         redshift of the source, if the design recorded one
 * @param spectrumFootprint expected extent of the spectrum on the detector, if the design
 *                          recorded one
 */
case class MosMaskSlit(
  id:                MosObjectId,
  coordinates:       Coordinates,
  x:                 Double,
  y:                 Double,
  magnitude:         BrightnessValue,
  slitWidth:         Angle,
  slitLength:        Angle,
  offsetAlongSlit:   Angle,
  offsetAcrossSlit:  Angle,
  tilt:              Angle,
  slitType:          MosSlitType,
  priority:          MosSlitPriority,
  redshift:          Option[Redshift],
  spectrumFootprint: Option[MosSpectrumFootprint]
) derives Eq:

  /** True if this slit targets an acquisition star rather than a science object. */
  def isAcquisition: Boolean =
    priority === MosSlitPriority.Acquisition

object MosMaskSlit:

  given Show[MosMaskSlit] =
    Show.fromToString

  /** @group Optics */
  val id: Lens[MosMaskSlit, MosObjectId] =
    Focus[MosMaskSlit](_.id)

  /** @group Optics */
  val coordinates: Lens[MosMaskSlit, Coordinates] =
    Focus[MosMaskSlit](_.coordinates)

  /** @group Optics */
  val x: Lens[MosMaskSlit, Double] =
    Focus[MosMaskSlit](_.x)

  /** @group Optics */
  val y: Lens[MosMaskSlit, Double] =
    Focus[MosMaskSlit](_.y)

  /** @group Optics */
  val magnitude: Lens[MosMaskSlit, BrightnessValue] =
    Focus[MosMaskSlit](_.magnitude)

  /** @group Optics */
  val slitWidth: Lens[MosMaskSlit, Angle] =
    Focus[MosMaskSlit](_.slitWidth)

  /** @group Optics */
  val slitLength: Lens[MosMaskSlit, Angle] =
    Focus[MosMaskSlit](_.slitLength)

  /** @group Optics */
  val offsetAlongSlit: Lens[MosMaskSlit, Angle] =
    Focus[MosMaskSlit](_.offsetAlongSlit)

  /** @group Optics */
  val offsetAcrossSlit: Lens[MosMaskSlit, Angle] =
    Focus[MosMaskSlit](_.offsetAcrossSlit)

  /** @group Optics */
  val tilt: Lens[MosMaskSlit, Angle] =
    Focus[MosMaskSlit](_.tilt)

  /** @group Optics */
  val slitType: Lens[MosMaskSlit, MosSlitType] =
    Focus[MosMaskSlit](_.slitType)

  /** @group Optics */
  val priority: Lens[MosMaskSlit, MosSlitPriority] =
    Focus[MosMaskSlit](_.priority)

  /** @group Optics */
  val redshift: Lens[MosMaskSlit, Option[Redshift]] =
    Focus[MosMaskSlit](_.redshift)

  /** @group Optics */
  val spectrumFootprint: Lens[MosMaskSlit, Option[MosSpectrumFootprint]] =
    Focus[MosMaskSlit](_.spectrumFootprint)

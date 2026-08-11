// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.mos

import cats.Eq
import cats.Show
import lucuma.core.math.Wavelength
import monocle.Focus
import monocle.Lens

/**
 * The spectroscopic configuration a MOS mask was designed against.
 *
 * A mask is cut for a particular filter, grating and central wavelength: those choices determine
 * how long each spectrum is and therefore how densely slits can be packed. The configuration is
 * recorded in the design so that the observation can be checked against it.
 *
 * Filter and grating are carried as raw strings rather than as instrument enumerations because a
 * mask file may name a configuration this library does not know, and rejecting the whole file over
 * an unrecognised grating would be unhelpful.
 *
 * @param filter            filter used for the spectroscopic observation
 * @param grating           grating or grism
 * @param centralWavelength requested central wavelength
 * @param minWavelength     shortest wavelength reaching the detector
 * @param maxWavelength     longest wavelength reaching the detector
 * @param dispersion        dispersion in nanometres per pixel
 * @param spectrumLength    length of a spectrum on the detector, in pixels
 * @param anamorphicFactor  anamorphic magnification of the spectrograph
 */
case class MosSpectroscopyConfig(
  filter:            Option[String],
  grating:           Option[String],
  centralWavelength: Option[Wavelength],
  minWavelength:     Option[Wavelength],
  maxWavelength:     Option[Wavelength],
  dispersion:        Option[Double],
  spectrumLength:    Option[Double],
  anamorphicFactor:  Option[Double]
)

object MosSpectroscopyConfig:

  val Empty: MosSpectroscopyConfig =
    MosSpectroscopyConfig(None, None, None, None, None, None, None, None)

  given Eq[MosSpectroscopyConfig] =
    Eq.by(c =>
      (c.filter,
       c.grating,
       c.centralWavelength,
       c.minWavelength,
       c.maxWavelength,
       c.dispersion,
       c.spectrumLength,
       c.anamorphicFactor
      )
    )

  given Show[MosSpectroscopyConfig] =
    Show.fromToString

  /** @group Optics */
  val filter: Lens[MosSpectroscopyConfig, Option[String]] =
    Focus[MosSpectroscopyConfig](_.filter)

  /** @group Optics */
  val grating: Lens[MosSpectroscopyConfig, Option[String]] =
    Focus[MosSpectroscopyConfig](_.grating)

  /** @group Optics */
  val centralWavelength: Lens[MosSpectroscopyConfig, Option[Wavelength]] =
    Focus[MosSpectroscopyConfig](_.centralWavelength)

  /** @group Optics */
  val minWavelength: Lens[MosSpectroscopyConfig, Option[Wavelength]] =
    Focus[MosSpectroscopyConfig](_.minWavelength)

  /** @group Optics */
  val maxWavelength: Lens[MosSpectroscopyConfig, Option[Wavelength]] =
    Focus[MosSpectroscopyConfig](_.maxWavelength)

  /** @group Optics */
  val dispersion: Lens[MosSpectroscopyConfig, Option[Double]] =
    Focus[MosSpectroscopyConfig](_.dispersion)

  /** @group Optics */
  val spectrumLength: Lens[MosSpectroscopyConfig, Option[Double]] =
    Focus[MosSpectroscopyConfig](_.spectrumLength)

  /** @group Optics */
  val anamorphicFactor: Lens[MosSpectroscopyConfig, Option[Double]] =
    Focus[MosSpectroscopyConfig](_.anamorphicFactor)

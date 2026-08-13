// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.mos

import cats.Eq
import cats.Show
import cats.derived.*
import coulomb.Quantity
import coulomb.integrations.cats.quantity.given
import lucuma.core.math.Wavelength
import lucuma.core.math.units.NanometersPerPixel
import lucuma.core.math.units.Pixels
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
 * @param grating           grating or grism
 * @param minWavelength     shortest wavelength reaching the detector
 * @param maxWavelength     longest wavelength reaching the detector
 * @param spectrumLength    length of a spectrum on the detector
 * @param anamorphicFactor  anamorphic magnification of the spectrograph
 */
case class MosSpectroscopyConfig(
  filter:            Option[String],
  grating:           Option[String],
  centralWavelength: Option[Wavelength],
  minWavelength:     Option[Wavelength],
  maxWavelength:     Option[Wavelength],
  dispersion:        Option[Quantity[BigDecimal, NanometersPerPixel]],
  spectrumLength:    Option[Quantity[BigDecimal, Pixels]],
  anamorphicFactor:  Option[Double]
) derives Eq

object MosSpectroscopyConfig:

  val Empty: MosSpectroscopyConfig =
    MosSpectroscopyConfig(None, None, None, None, None, None, None, None)

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
  val dispersion: Lens[MosSpectroscopyConfig, Option[Quantity[BigDecimal, NanometersPerPixel]]] =
    Focus[MosSpectroscopyConfig](_.dispersion)

  /** @group Optics */
  val spectrumLength: Lens[MosSpectroscopyConfig, Option[Quantity[BigDecimal, Pixels]]] =
    Focus[MosSpectroscopyConfig](_.spectrumLength)

  /** @group Optics */
  val anamorphicFactor: Lens[MosSpectroscopyConfig, Option[Double]] =
    Focus[MosSpectroscopyConfig](_.anamorphicFactor)

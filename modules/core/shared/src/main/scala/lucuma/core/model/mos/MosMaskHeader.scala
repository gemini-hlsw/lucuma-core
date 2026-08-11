// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.mos

import cats.Eq
import cats.Show
import lucuma.core.enums.Instrument
import lucuma.core.enums.MosDispersionDirection
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import monocle.Focus
import monocle.Lens

/**
 * File level metadata of a MOS mask design.
 *
 * `keywords` retains every keyword found in the file, including those given a typed field above.
 * This is deliberate: the format has drifted across versions of the mask design software, so a
 * reader must be able to reach a keyword this library does not yet model without waiting for a
 * release. Where a keyword appears more than once — which real files do contain — the first
 * occurrence is the one retained.
 *
 * `pixelScale` is nominal rather than measured. Mask design software snaps it to a fixed value per
 * detector configuration, because the true scale is not constant across the field and downstream
 * tooling requires a stable number.
 *
 * @param instrument          instrument the mask was designed for
 * @param dispersionDirection axis along which spectra are spread; determines how slit dimensions
 *                            map onto the file's x and y columns
 * @param pixelScale          nominal pre-image plate scale, in arcseconds per pixel
 * @param pointing            pointing centre of the pre-image
 * @param positionAngle       position angle the mask must be observed at; absent in designs
 *                            produced before mask design software began recording it
 * @param hasTiltedSlits      whether any slit in the design is tilted
 * @param nodAndShuffle       Nod & Shuffle configuration, if any
 * @param spectroscopy        the spectroscopic setup the design assumes
 * @param provenance          where the design came from
 * @param keywords            every keyword in the file, as raw strings
 */
case class MosMaskHeader(
  instrument:          Instrument,
  dispersionDirection: MosDispersionDirection,
  pixelScale:          Double,
  pointing:            Coordinates,
  positionAngle:       Option[Angle],
  hasTiltedSlits:      Boolean,
  nodAndShuffle:       MosNodAndShuffle,
  spectroscopy:        MosSpectroscopyConfig,
  provenance:          MosMaskProvenance,
  keywords:            Map[String, String]
):

  /** The raw value of `key`, if the file carried it. */
  def keyword(key: String): Option[String] =
    keywords.get(key)

object MosMaskHeader:

  given Eq[MosMaskHeader] =
    Eq.by(h =>
      (h.instrument,
       h.dispersionDirection,
       h.pixelScale,
       h.pointing,
       h.positionAngle,
       h.hasTiltedSlits,
       h.nodAndShuffle,
       h.spectroscopy,
       h.provenance,
       h.keywords
      )
    )

  given Show[MosMaskHeader] =
    Show.fromToString

  /** @group Optics */
  val instrument: Lens[MosMaskHeader, Instrument] =
    Focus[MosMaskHeader](_.instrument)

  /** @group Optics */
  val dispersionDirection: Lens[MosMaskHeader, MosDispersionDirection] =
    Focus[MosMaskHeader](_.dispersionDirection)

  /** @group Optics */
  val pixelScale: Lens[MosMaskHeader, Double] =
    Focus[MosMaskHeader](_.pixelScale)

  /** @group Optics */
  val pointing: Lens[MosMaskHeader, Coordinates] =
    Focus[MosMaskHeader](_.pointing)

  /** @group Optics */
  val positionAngle: Lens[MosMaskHeader, Option[Angle]] =
    Focus[MosMaskHeader](_.positionAngle)

  /** @group Optics */
  val hasTiltedSlits: Lens[MosMaskHeader, Boolean] =
    Focus[MosMaskHeader](_.hasTiltedSlits)

  /** @group Optics */
  val nodAndShuffle: Lens[MosMaskHeader, MosNodAndShuffle] =
    Focus[MosMaskHeader](_.nodAndShuffle)

  /** @group Optics */
  val spectroscopy: Lens[MosMaskHeader, MosSpectroscopyConfig] =
    Focus[MosMaskHeader](_.spectroscopy)

  /** @group Optics */
  val provenance: Lens[MosMaskHeader, MosMaskProvenance] =
    Focus[MosMaskHeader](_.provenance)

  /** @group Optics */
  val keywords: Lens[MosMaskHeader, Map[String, String]] =
    Focus[MosMaskHeader](_.keywords)

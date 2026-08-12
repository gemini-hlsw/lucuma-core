// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.mos

import cats.Eq
import cats.Show
import cats.derived.*
import lucuma.core.util.Timestamp
import monocle.Focus
import monocle.Lens

/**
 * Where a MOS mask design came from.
 *
 * `softwareVersion` matters more than it looks: the mask file format has changed repeatedly, and
 * the version stamp is how a reader tells which columns and keywords to expect from a given file.
 *
 * @param softwareVersion        version of the mask design software that produced the file
 * @param designer               account name of the person who ran the design
 * @param designedAt             when the design was produced
 * @param sourceObjectTable      file name of the object table the design was built from
 * @param detectorIdImaging      identifier of the detector used for the pre-image
 * @param detectorIdSpectroscopy identifier of the detector the spectra are planned for
 */
case class MosMaskProvenance(
  softwareVersion:        Option[String],
  designer:               Option[String],
  designedAt:             Option[Timestamp],
  sourceObjectTable:      Option[String],
  detectorIdImaging:      Option[String],
  detectorIdSpectroscopy: Option[String]
) derives Eq

object MosMaskProvenance:

  val Empty: MosMaskProvenance =
    MosMaskProvenance(None, None, None, None, None, None)

  given Show[MosMaskProvenance] =
    Show.fromToString

  /** @group Optics */
  val softwareVersion: Lens[MosMaskProvenance, Option[String]] =
    Focus[MosMaskProvenance](_.softwareVersion)

  /** @group Optics */
  val designer: Lens[MosMaskProvenance, Option[String]] =
    Focus[MosMaskProvenance](_.designer)

  /** @group Optics */
  val designedAt: Lens[MosMaskProvenance, Option[Timestamp]] =
    Focus[MosMaskProvenance](_.designedAt)

  /** @group Optics */
  val sourceObjectTable: Lens[MosMaskProvenance, Option[String]] =
    Focus[MosMaskProvenance](_.sourceObjectTable)

  /** @group Optics */
  val detectorIdImaging: Lens[MosMaskProvenance, Option[String]] =
    Focus[MosMaskProvenance](_.detectorIdImaging)

  /** @group Optics */
  val detectorIdSpectroscopy: Lens[MosMaskProvenance, Option[String]] =
    Focus[MosMaskProvenance](_.detectorIdSpectroscopy)

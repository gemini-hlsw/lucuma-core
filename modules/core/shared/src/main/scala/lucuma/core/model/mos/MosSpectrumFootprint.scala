// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.mos

import cats.Eq
import cats.Show
import cats.derived.*
import monocle.Focus
import monocle.Lens

/**
 * The rectangle of pre-image pixels a slit's spectrum is expected to occupy on the detector.
 *
 * Mask design software uses these bounds to detect whether two slits' spectra would overlap. The
 * edges are given in pre-image pixel coordinates, so which pair spans the dispersion axis depends
 * on the instrument: for a horizontally dispersing instrument `left`/`right` span the spectrum and
 * `bottom`/`top` span the slit itself, and vice versa for a vertically dispersing one.
 *
 * This is optional in a mask file — it was added to the format after the fact, so older files
 * legitimately lack it.
 */
case class MosSpectrumFootprint(
  left:   Double,
  right:  Double,
  bottom: Double,
  top:    Double
) derives Eq

object MosSpectrumFootprint:

  given Show[MosSpectrumFootprint] =
    Show.fromToString

  /** @group Optics */
  val left: Lens[MosSpectrumFootprint, Double] =
    Focus[MosSpectrumFootprint](_.left)

  /** @group Optics */
  val right: Lens[MosSpectrumFootprint, Double] =
    Focus[MosSpectrumFootprint](_.right)

  /** @group Optics */
  val bottom: Lens[MosSpectrumFootprint, Double] =
    Focus[MosSpectrumFootprint](_.bottom)

  /** @group Optics */
  val top: Lens[MosSpectrumFootprint, Double] =
    Focus[MosSpectrumFootprint](_.top)

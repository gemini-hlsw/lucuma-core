// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.eq.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import monocle.Prism

/**
 * Shape of an aperture cut in a MOS mask.
 *
 * Only rectangular slits are supported by Gemini's mask cutting. A tilted slit is still
 * `Rectangular` — it is represented as a parallelogram whose width in the dispersion direction is
 * unchanged, so spectral resolution is preserved. Curved slits do not exist; they are approximated
 * by concatenating neighbouring slits at different tilts.
 *
 * @group Enumerations
 */
enum MosSlitType(
  val tag:       String,
  val fitsValue: Char,
  val shortName: String,
  val longName:  String
) derives Enumerated:

  case Rectangular extends MosSlitType("rectangular", 'R', "R", "Rectangular")

object MosSlitType:

  /** The single character used to encode a slit type in a MOS mask file. */
  val fromFitsValue: Prism[Char, MosSlitType] =
    Prism[Char, MosSlitType](c => values.find(_.fitsValue === c))(_.fitsValue)

  given Display[MosSlitType] =
    Display.by(_.shortName, _.longName)

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Shape of an aperture cut in a MOS mask.
 *
 * Only rectangular slits are supported by Gemini's mask cutting. A tilted slit is still
 * `Rectangular` — it is represented as a parallelogram whose width in the dispersion direction is
 * unchanged, so spectral resolution is preserved. Curved slits do not exist; they are approximated
 * by concatenating neighbouring slits at different tilts.
 *
 * Tags are the single characters used to encode a slit type in a MOS mask file.
 *
 * @group Enumerations
 */
enum MosSlitType(
  val tag:      String,
  val longName: String
) derives Enumerated:

  case Rectangular extends MosSlitType("R", "Rectangular")

object MosSlitType:

  given Display[MosSlitType] =
    Display.by(_.tag, _.longName)

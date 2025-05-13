// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for Flamingos2 decker option.
 * @group Enumerations
 */
enum Flamingos2Decker(val tag: String, val shortName: String, val longName: String)
  derives Enumerated:

  case Imaging  extends Flamingos2Decker("Imaging",  "Imaging",   "Imaging")
  case LongSlit extends Flamingos2Decker("LongSlit", "Long Slit", "LongSlit")
  case MOS      extends Flamingos2Decker("MOS",      "MOS",       "MOS")


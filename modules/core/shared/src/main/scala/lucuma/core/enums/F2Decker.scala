// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for Flamingos2 decker option.
 * @group Enumerations
 */
enum F2Decker(val tag: String, val shortName: String, val longName: String, val obsolete: Boolean)
  derives Enumerated:

  case Imaging extends F2Decker("Imaging", "Imaging", "Imaging", false)
  case LongSlit extends F2Decker("LongSlit", "Long Slit", "LongSlit", false)
  case MOS extends F2Decker("MOS", "MOS", "MOS", false)

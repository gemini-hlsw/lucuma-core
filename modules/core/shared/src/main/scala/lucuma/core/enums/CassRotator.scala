// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Enumerated type for possible cass rotator modes.
 */
enum CassRotator(val tag: String, val shortName: String) derives Enumerated:

  case Fixed     extends CassRotator("fixed", "Fixed")
  case Following extends CassRotator("following", "Following")

object CassRotator:
  given Display[CassRotator] = Display.byShortName(_.shortName)

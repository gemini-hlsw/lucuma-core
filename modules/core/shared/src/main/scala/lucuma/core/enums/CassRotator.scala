// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for possible cass rotator modes.
 */
enum CassRotator(val tag: String) derives Enumerated:

  case Fixed     extends CassRotator("fixed")
  case Following extends CassRotator("following")

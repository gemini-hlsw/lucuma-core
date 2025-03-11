// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for track types.
 * @group Enumerations (Generated)
 */
enum TrackType(
  val tag: String
) derives Enumerated:
  case Sidereal extends TrackType("Sidereal")
  case Nonsidereal extends TrackType("Nonsidereal")

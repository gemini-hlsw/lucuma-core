// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GNIRS Disperser.
 * @group Enumerations (Generated)
 */
enum GnirsGrating(
  val tag: String,
  val shortName: String,
  val longName: String,
  val rulingDensity: Int
) derives Enumerated, Display:
  case D10 extends GnirsGrating("D10", "10", "10 l/mm grating", 10)
  case D32 extends GnirsGrating("D32", "32", "32 l/mm grating", 32)
  case D111 extends GnirsGrating("D111", "111", "111 l/mm grating", 111)

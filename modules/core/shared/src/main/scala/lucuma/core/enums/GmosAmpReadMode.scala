// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS amp read mode.
 * @group Enumerations
 */
enum GmosAmpReadMode(
  val tag: String,
  val shortName: String,
  val longName: String
) derives Enumerated:

  case Slow extends GmosAmpReadMode("Slow", "slow", "Slow")
  case Fast extends GmosAmpReadMode("Fast", "fast", "Fast")

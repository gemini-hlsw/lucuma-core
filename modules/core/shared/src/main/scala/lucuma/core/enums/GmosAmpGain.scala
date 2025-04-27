// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS amp gain.
 * @group Enumerations
 */
enum GmosAmpGain(
  val tag: String,
  val shortName: String,
  val longName: String
) derives Enumerated:

  case Low  extends GmosAmpGain("Low",  "Low",  "Low")
  case High extends GmosAmpGain("High", "High", "High")

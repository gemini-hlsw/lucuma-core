// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS X-binning.
 * @group Enumerations (Generated)
 */
enum GmosXBinning(
  val tag: String,
  val shortName: String,
  val longName: String,
  val count: Int
) derives Enumerated:
  case One extends GmosXBinning("One", "1", "One", 1)
  case Two extends GmosXBinning("Two", "2", "Two", 2)
  case Four extends GmosXBinning("Four", "4", "Four", 4)

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.telluric

import lucuma.core.math.Coordinates
import lucuma.core.model.TelluricType
import lucuma.core.util.TimeSpan

case class TelluricSearchInput(
  coordinates: Coordinates,
  duration:    TimeSpan,
  brightest:   BigDecimal,
  spType:      TelluricType
)

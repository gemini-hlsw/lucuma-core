// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type of conditions measurement sources.
 */
enum ConditionsMeasurementSource(val tag: String, val name: String) derives Enumerated:
  case Observer extends ConditionsMeasurementSource("observer", "Observer")

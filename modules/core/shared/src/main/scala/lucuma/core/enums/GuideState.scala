// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * An enumeration whose value determines whether guiding is enabled for a particular step.
 */
enum GuideState(val tag: String, val name: String, val description: String) derives Enumerated {

  case Enabled  extends GuideState("enabled",  "Enabled",  "Guiding is enabled.")
  case Disabled extends GuideState("disabled", "Disabled", "Guiding is disabled.")

}

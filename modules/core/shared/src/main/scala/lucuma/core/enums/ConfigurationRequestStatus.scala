// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated


enum ConfigurationRequestStatus(val tag: String, val name: String) derives Enumerated, Display:
  case Requested extends ConfigurationRequestStatus("requested", "Requested")
  case Approved  extends ConfigurationRequestStatus("approved", "Approved")
  case Denied    extends ConfigurationRequestStatus("denied", "Denied")
  case Withdrawn extends ConfigurationRequestStatus("withdrawn", "Withdrawn")

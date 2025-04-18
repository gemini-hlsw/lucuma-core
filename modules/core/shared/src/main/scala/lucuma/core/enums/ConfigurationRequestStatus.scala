// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated


enum ConfigurationRequestStatus(val tag: String) derives Enumerated:
  case Requested extends ConfigurationRequestStatus("requested")
  case Approved  extends ConfigurationRequestStatus("approved")
  case Denied    extends ConfigurationRequestStatus("denied")
  case Withdrawn extends ConfigurationRequestStatus("withdrawn")

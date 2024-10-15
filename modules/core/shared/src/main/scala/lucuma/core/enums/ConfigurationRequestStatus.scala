// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated


enum ConfigurationRequestStatus(val tag: String) derives Enumerated:
  case Requested extends ConfigurationRequestStatus("Requested")
  case Approved  extends ConfigurationRequestStatus("Approved")
  case Denied    extends ConfigurationRequestStatus("Denied")
  case Withdrawn extends ConfigurationRequestStatus("Withdrawn")

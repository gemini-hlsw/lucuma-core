// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.util.Enumerated
import lucuma.core.util.WithGid
import lucuma.refined.*

case class ConfigurationRequest(
  id: ConfigurationRequest.Id,
  status: ConfigurationRequest.Status,
  configuration: Configuration
)

object ConfigurationRequest extends WithGid('x'.refined) {
  enum Status(val tag: String) derives Enumerated:
    case Requested extends Status("Requested")
    case Approved  extends Status("Approved")
    case Denied    extends Status("Denied")
    case Withdrawn extends Status("Withdrawn")
}

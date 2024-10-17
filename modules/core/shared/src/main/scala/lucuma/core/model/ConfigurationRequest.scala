// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.derived.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.util.WithGid
import lucuma.refined.*

case class ConfigurationRequest(
  id: ConfigurationRequest.Id,
  status: ConfigurationRequestStatus,
  configuration: Configuration
) derives Eq

object ConfigurationRequest extends WithGid('x'.refined)

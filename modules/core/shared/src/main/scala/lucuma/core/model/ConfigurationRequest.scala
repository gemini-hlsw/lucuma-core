// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.util.WithGid
import lucuma.refined.*
import monocle.Focus
import monocle.Lens

case class ConfigurationRequest(
  id:            ConfigurationRequest.Id,
  status:        ConfigurationRequestStatus,
  justification: Option[NonEmptyString],
  configuration: Configuration
) derives Eq

object ConfigurationRequest extends WithGid('x'.refined):
  val id: Lens[ConfigurationRequest, ConfigurationRequest.Id]        = Focus[ConfigurationRequest](_.id)
  val status: Lens[ConfigurationRequest, ConfigurationRequestStatus] =
    Focus[ConfigurationRequest](_.status)
  val justification: Lens[ConfigurationRequest, Option[NonEmptyString]] =
    Focus[ConfigurationRequest](_.justification)
  val configuration: Lens[ConfigurationRequest, Configuration]       =
    Focus[ConfigurationRequest](_.configuration)

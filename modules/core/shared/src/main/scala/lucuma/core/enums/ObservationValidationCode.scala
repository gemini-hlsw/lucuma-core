// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum ObservationValidationCode(
  val tag: String,
  val name: String,
  val description: String
) derives Enumerated:
  case ConfigurationError extends ObservationValidationCode("configuration_error", "Configuration Error", "The observation is not configured correctly and cannot be executed")
  case CallForProposalsError extends ObservationValidationCode("cfp_error", "Call for Proposals Error", "Not valid for the selected Call for Proposals")
  case ItcError extends ObservationValidationCode("itc_error", "ITC Error", "Integration time is unavailable.")
  case ConfigurationRequestError extends ObservationValidationCode("configuration_error", "Configuration Error", "This observation's configuration is not approved for execution.")
// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

import ObservationValidationCode.Severity
import ObservationValidationCode.Severity.Fatal

enum ObservationValidationCode(
  val tag: String,
  val name: String,
  val description: String,
  val severity: Severity
) derives Enumerated:
  case ConfigurationError extends ObservationValidationCode("configuration_error", "Configuration Error", "The observation is not configured correctly and cannot be executed", Fatal)
  case CallForProposalsError extends ObservationValidationCode("cfp_error", "Call for Proposals Error", "Not valid for the selected Call for Proposals", Fatal)
  case ItcError extends ObservationValidationCode("itc_error", "ITC Error", "Integration time is unavailable.", Fatal)
  case ConfigurationRequestUnavailable 
    extends ObservationValidationCode(
      "config_request_unavailable", 
      "Unknown Approval Status", 
      ObservationValidationCode.ConfigurationRequestMsg.Unavailable, 
      Fatal
    )
  case ConfigurationRequestNotRequested
    extends ObservationValidationCode(
      "config_request_not_requested", 
      "Needs Approval", 
      ObservationValidationCode.ConfigurationRequestMsg.NotRequested, 
      Fatal
    )
  case ConfigurationRequestDenied
    extends ObservationValidationCode(
      "config_request_denied", 
      "Denied", 
      ObservationValidationCode.ConfigurationRequestMsg.Denied, 
      Fatal
    )
  case ConfigurationRequestPending
    extends ObservationValidationCode(
      "config_request_pending",
      "Approval Pending",
      ObservationValidationCode.ConfigurationRequestMsg.Pending,
      Fatal
    )
  case TooActivationUnapproved
    extends ObservationValidationCode(
      "too_activation_unapproved",
      "ToO Activation Unapproved",
      ObservationValidationCode.TooActivationMsg.ExceedsCeiling,
      Fatal
    )

object ObservationValidationCode:

  enum Severity:
    case Fatal, Nonfatal
  object TooActivationMsg:
    val ExceedsCeiling = "Target of Opportunity activation exceeds what the accepted proposal allows."

  object ConfigurationRequestMsg:
    val Unavailable  = "Configuration approval status could not be determined."
    val NotRequested = "Configuration is unapproved (approval has not been requested)."
    val Denied       = "Configuration is unapproved (request was denied)."
    val Pending      = "Configuration is unapproved (request is pending)."

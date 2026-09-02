// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

import ObservationValidationCode.*

enum ObservationValidationCode(
  val tag: String,
  val name: String,
  val description: String,
) derives Enumerated:
  def severity: Severity
  case ConfigurationError extends ObservationValidationCode("configuration_error", "Configuration Error", "The observation is not configured correctly and cannot be executed") with Fatal
  case CallForProposalsError extends ObservationValidationCode("cfp_error", "Call for Proposals Error", "Not valid for the selected Call for Proposals") with Fatal
  case ItcError extends ObservationValidationCode("itc_error", "ITC Error", "Integration time is unavailable.") with Fatal
  case ConfigurationRequestUnavailable 
    extends ObservationValidationCode(
      "config_request_unavailable", 
      "Unknown Approval Status", 
      ObservationValidationCode.ConfigurationRequestMsg.Unavailable, 
    ) with Fatal
  case ConfigurationRequestNotRequested
    extends ObservationValidationCode(
      "config_request_not_requested", 
      "Needs Approval", 
      ObservationValidationCode.ConfigurationRequestMsg.NotRequested, 
    ) with Fatal
  case ConfigurationRequestDenied
    extends ObservationValidationCode(
      "config_request_denied", 
      "Denied", 
      ObservationValidationCode.ConfigurationRequestMsg.Denied, 
    ) with Fatal
  case ConfigurationRequestPending
    extends ObservationValidationCode(
      "config_request_pending",
      "Approval Pending",
      ObservationValidationCode.ConfigurationRequestMsg.Pending,
    ) with Fatal
  case TooActivationUnapproved
    extends ObservationValidationCode(
      "too_activation_unapproved",
      "ToO Activation Unapproved",
      ObservationValidationCode.TooActivationMsg.ExceedsCeiling,
    ) with Fatal
  @deprecated("Use a fine-grained code.")
  case GenericWarning
    extends ObservationValidationCode(
      "generic_warning",
      "Warning",
      "A warning was issued.",
    ) with Nonfatal
  case ConditionsUnlikely
    extends ObservationValidationCode(
      "conditions_unlikely",
      "Observing Conditions Unlikely",
      "Likelihood of observing conditions is below recommended percentage.",
    ) with Nonfatal
  case LowTotalSignalToNoise
    extends ObservationValidationCode(
      "low_total_signal_to_noise",
      "Low Total Signal to Noise",
      "Total signal to noise is below recommended threshold.",
    ) with Nonfatal

object ObservationValidationCode:

  // Marker traits, so you can have a catch-all match case for wanings without
  // losing exhaustiveness checks for fatal. This is important in the workflow
  // calculation in the ODB.
  trait Fatal:
    def severity = Severity.Fatal
  trait Nonfatal:
    def severity = Severity.Nonfatal

  enum Severity:
    case Fatal, Nonfatal
  object TooActivationMsg:
    val ExceedsCeiling = "Target of Opportunity activation exceeds what the accepted proposal allows."

  object ConfigurationRequestMsg:
    val Unavailable  = "Configuration approval status could not be determined."
    val NotRequested = "Configuration is unapproved (approval has not been requested)."
    val Denied       = "Configuration is unapproved (request was denied)."
    val Pending      = "Configuration is unapproved (request is pending)."

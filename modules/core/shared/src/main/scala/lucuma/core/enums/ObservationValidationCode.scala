// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumeration identifying classes of validation problems. The top-level enumeration `ObservationValidationCode` is
 * also available as a pair of sub-enumerations `ObservationValidationCode.Error` and 
 * `ObservationValidationCode.Warning` for code that needs to distinguish these at the type level.
 */

sealed trait ObservationValidationCode:
  import ObservationValidationCode.*

  def tag: String // we must ensure th
  def name: String
  def description: String
  
  def fold[A](f: Error => A, g: Warning => A): A =
    this match
      case e: Error   => f(e)
      case w: Warning => g(w)

  def isError: Boolean = fold(_ => true, _ => false)
  def asError: Option[Error] = fold(Some(_), _ => None)

  def isWarning: Boolean = fold(_ => false, _ => true)
  def asWarning: Option[Warning] = fold(_ => None, Some(_))

  @deprecated("Use fold or a variant.")
  def severity: Severity = fold(_ => Severity.Fatal, _ => Severity.Nonfatal)
 
object ObservationValidationCode:
  export Error.*
  export Warning.*

  // TODO: remove
  enum Severity:
    case Fatal, Nonfatal

  @deprecated("Use `Error`")
  type Fatal = Error

  @deprecated("Use `Warning`")
  type Nonfatal = Warning

  given Enumerated[ObservationValidationCode] with
    assert:   // ensure that the tag sets are disjoint
      val te = Enumerated[Error].all.map(_.tag).toSet
      val tw = Enumerated[Warning].all.map(_.tag).toSet
      te.intersect(tw).isEmpty
    def all: List[ObservationValidationCode] = Enumerated[Error].all ++ Enumerated[Warning].all
    def tag(a: ObservationValidationCode): String = a.tag

  enum Error(
    val tag: String,
    val name: String,
    val description: String,
  ) extends ObservationValidationCode derives Enumerated:

    case ConfigurationError extends Error("configuration_error", "Configuration Error", "The observation is not configured correctly and cannot be executed") 
    case CallForProposalsError extends Error("cfp_error", "Call for Proposals Error", "Not valid for the selected Call for Proposals") 
    case ItcError extends Error("itc_error", "ITC Error", "Integration time is unavailable.") 
    case ConfigurationRequestUnavailable 
      extends Error(
        "config_request_unavailable", 
        "Unknown Approval Status", 
        Error.ConfigurationRequestMsg.Unavailable, 
      ) 
    case ConfigurationRequestNotRequested
      extends Error(
        "config_request_not_requested", 
        "Needs Approval", 
        Error.ConfigurationRequestMsg.NotRequested, 
      ) 
    case ConfigurationRequestDenied
      extends Error(
        "config_request_denied", 
        "Denied", 
        Error.ConfigurationRequestMsg.Denied, 
      ) 
    case ConfigurationRequestPending
      extends Error(
        "config_request_pending",
        "Approval Pending",
        Error.ConfigurationRequestMsg.Pending,
      ) 
    case TooActivationUnapproved
      extends Error(
        "too_activation_unapproved",
        "ToO Activation Unapproved",
        Error.TooActivationMsg.ExceedsCeiling,
      ) 

  object Error:
    object TooActivationMsg:
      val ExceedsCeiling = "Target of Opportunity activation exceeds what the accepted proposal allows."

    object ConfigurationRequestMsg:
      val Unavailable  = "Configuration approval status could not be determined."
      val NotRequested = "Configuration is unapproved (approval has not been requested)."
      val Denied       = "Configuration is unapproved (request was denied)."
      val Pending      = "Configuration is unapproved (request is pending)."

  enum Warning(
    val tag: String,
    val name: String,
    val description: String,
  ) extends ObservationValidationCode derives Enumerated:

    @deprecated("Use a fine-grained code.")
    case GenericWarning
      extends Warning(
        "generic_warning",
        "Warning",
        "A warning was issued.",
      ) 
    case ConditionsUnlikely
      extends Warning(
        "conditions_unlikely",
        "Observing Conditions Unlikely",
        "Likelihood of observing conditions is below recommended percentage.",
      ) 
    case LowTotalSignalToNoise
      extends Warning(
        "low_total_signal_to_noise",
        "Low Total Signal to Noise",
        "Total signal to noise is below recommended threshold.",
      ) 



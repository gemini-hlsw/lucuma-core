// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.data.NonEmptyChain
import cats.derived.*
import cats.syntax.all.*
import io.circe.Codec
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.ObservationValidationCode.*
import lucuma.core.math.TotalSN

case class ObservationValidation(
  code: ObservationValidationCode,
  messages: NonEmptyChain[String]
) derives Codec, Eq

object ObservationValidation:
  def fromMsgs(code: ObservationValidationCode, msg: String, moreMsgs: String*): ObservationValidation =
    ObservationValidation(code, NonEmptyChain.of(msg, moreMsgs*))
  def configuration(msg: String, moreMsgs: String*): ObservationValidation =
    fromMsgs(ConfigurationError, msg, moreMsgs*)
  def callForProposals(msg: String, moreMsgs: String*): ObservationValidation =
    fromMsgs(CallForProposalsError, msg, moreMsgs*)
  def itc(msg: String, moreMsgs: String*): ObservationValidation =
    fromMsgs(ItcError, msg, moreMsgs*)
  def configurationRequestUnavailable: ObservationValidation =
    fromMsgs(ConfigurationRequestUnavailable, ConfigurationRequestUnavailable.description)
  def configurationRequestNotRequested: ObservationValidation =
    fromMsgs(ConfigurationRequestNotRequested, ConfigurationRequestNotRequested.description)
  def configurationRequestDenied: ObservationValidation =
    fromMsgs(ConfigurationRequestDenied, ConfigurationRequestDenied.description)
  def configurationRequestPending: ObservationValidation =
    fromMsgs(ConfigurationRequestPending, ConfigurationRequestPending.description)
  def tooActivationUnapproved(msg: String, moreMsgs: String*): ObservationValidation =
    fromMsgs(TooActivationUnapproved, msg, moreMsgs*)
  @deprecated("Use a fine-grained warning.")
  def genericWarning(msg: String, moreMsgs: String*): ObservationValidation =
    fromMsgs(GenericWarning, msg, moreMsgs*)
    
  object Warning:
    
    def conditionsUnlikely(recommended: IntCentiPercent, actual: IntCentiPercent): ObservationValidation =
      fromMsgs(ConditionsUnlikely, s"Conditions likelihood is ${actual.toPercent.toInt}%; minimum recommended is ${recommended.toPercent.toInt}%.")

    def lowTotalSignalToNoise(extra: Option[String], minRecommended: TotalSN, actual: TotalSN): ObservationValidation =
      fromMsgs(LowTotalSignalToNoise, f"Total S/N ${extra.foldMap(s => s"($s) ")} is ${actual.value.toBigDecimal}%4.3f (min. ${minRecommended.value.toBigDecimal}%4.3f recommended)")
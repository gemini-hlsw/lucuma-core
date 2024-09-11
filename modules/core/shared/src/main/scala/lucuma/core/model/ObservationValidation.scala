// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.data.NonEmptyChain
import cats.derived.*
import io.circe.Codec
import lucuma.core.enums.ObservationValidationCode

case class ObservationValidation(
  code: ObservationValidationCode,
  messages: NonEmptyChain[String]
) derives Codec, Eq

object ObservationValidation:
  def fromMsgs(code: ObservationValidationCode, msg: String, moreMsgs: String*): ObservationValidation =
    ObservationValidation(code, NonEmptyChain.of(msg, moreMsgs*))
  def configuration(msg: String, moreMsgs: String*): ObservationValidation =
    fromMsgs(ObservationValidationCode.ConfigurationError, msg, moreMsgs*)
  def callForProposals(msg: String, moreMsgs: String*): ObservationValidation =
    fromMsgs(ObservationValidationCode.CallForProposalsError, msg, moreMsgs*)
  def itc(msg: String, moreMsgs: String*): ObservationValidation =
    fromMsgs(ObservationValidationCode.ItcError, msg, moreMsgs*)
  def configuration(msg: String, moreMsgs: String*): ObservationValidation =
    fromMsgs(ObservationValidationCode.ConfigurationError, msg, moreMsgs*)

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.data.NonEmptyChain
import io.circe.Codec
import lucuma.core.enums.ObservationValidationCode

case class ObservationValidation(
  code: ObservationValidationCode,
  messages: NonEmptyChain[String]
) derives Codec

object ObservationValidation:
  def fromMsgs(code: ObservationValidationCode, msg: String, moreMsgs: String*): ObservationValidation =
    ObservationValidation(code, NonEmptyChain.of(msg, moreMsgs*))
  def configuration(msg: String, moreMsgs: String*): ObservationValidation =
    fromMsgs(ObservationValidationCode.ConfigurationError, msg, moreMsgs*)
  def callForProposals(msg: String, moreMsgs: String*): ObservationValidation =
    fromMsgs(ObservationValidationCode.CallForProposalsError, msg, moreMsgs*)

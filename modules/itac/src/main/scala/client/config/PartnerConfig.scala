// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package itac.config

import io.circe._
import io.circe.generic.semiauto._
import lucuma.core.model.IntCentiPercent
import lucuma.core.enums.Site

final case class PartnerConfig(
  email:   Email,
  percent: IntCentiPercent,
  sites:   List[Site]
)

object PartnerConfig {
  // implicit val encoderPartnerConfig: Encoder[PartnerConfig] = deriveEncoder
  // implicit val decoderPartnerConfig: Decoder[PartnerConfig] = deriveDecoder
}

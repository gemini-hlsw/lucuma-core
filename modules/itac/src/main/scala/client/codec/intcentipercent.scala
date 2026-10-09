// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package itac.codec

import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.model.IntCentiPercent

object intcentipercent:
  given Encoder[IntCentiPercent] = Encoder[BigDecimal].contramap(_.toPercent)
  given Decoder[IntCentiPercent] = Decoder[BigDecimal].emap(IntCentiPercent.fromPercent)


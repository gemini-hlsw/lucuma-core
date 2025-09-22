// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.circe.coulomb

import coulomb.Quantity
import coulomb.syntax.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.syntax.*

given [N: Decoder, U]: Decoder[Quantity[N, U]] = Decoder.instance(_.as[N].map(_.withUnit[U]))

given [N: Encoder, U]: Encoder[Quantity[N, U]] =
  Encoder.instance(_.value.asJson)

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.telluric

import io.circe.Encoder
import io.circe.Json
import io.circe.JsonObject
import io.circe.syntax.*
import lucuma.catalog.telluric.codecs.given
import lucuma.core.math.Coordinates
import lucuma.core.model.TelluricType
import lucuma.core.util.TimeSpan

case class TelluricSearchInput(
  coordinates: Coordinates,
  duration:    TimeSpan,
  brightest:   BigDecimal,
  spType:      TelluricType
)

object TelluricSearchInput:

  given Encoder.AsObject[TelluricSearchInput] = input =>
    JsonObject(
      "ra_deg"       -> Json.fromDoubleOrNull(input.coordinates.ra.toAngle.toDoubleDegrees),
      "dec_deg"      -> Json.fromDoubleOrNull(input.coordinates.dec.toAngle.toSignedDoubleDegrees),
      "duration_hrs" -> Json.fromDoubleOrNull(input.duration.toHours.toDouble),
      "brightest"    -> Json.fromBigDecimal(input.brightest),
      "sp_type"      -> input.spType.asJson
    )

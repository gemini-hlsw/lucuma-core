// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.telluric

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
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

  extension (tt: TelluricType)
    def toInput: String =
      tt match
        case TelluricType.Hot               => "hot"
        case TelluricType.A0V               => "A0V"
        case TelluricType.Solar             => "Solar"
        case TelluricType.Manual(starTypes) => starTypes.toList.mkString(",")

  given Encoder[TelluricSearchInput] = input =>
    Json.obj(
      "ra_deg"       -> Json.fromDoubleOrNull(input.coordinates.ra.toAngle.toDoubleDegrees),
      "dec_deg"      -> Json.fromDoubleOrNull(input.coordinates.dec.toAngle.toSignedDoubleDegrees),
      "duration_hrs" -> Json.fromDoubleOrNull(input.duration.toHours.toDouble),
      "brightest"    -> Json.fromBigDecimal(input.brightest),
      "sp_type"      -> input.spType.toInput.asJson
    )

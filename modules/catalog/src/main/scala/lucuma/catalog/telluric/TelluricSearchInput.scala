// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.telluric

import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Coordinates
import lucuma.core.util.TimeSpan

/**
 * Input parameters for searching telluric standard star candidates
 *
 * @param coordinates target coordinates
 * @param duration duration of observation
 * @param brightest brightest magnitude limit
 * @param spType spectral type (e.g., "A0V", "hot")
 */
case class TelluricSearchInput(
  coordinates: Coordinates,
  duration: TimeSpan,
  brightest: BigDecimal,
  spType: String
)

object TelluricSearchInput:
  given Encoder[TelluricSearchInput] = Encoder.instance { input =>
    Json.obj(
      "ra_deg" -> Json.fromDoubleOrNull(input.coordinates.ra.toAngle.toDoubleDegrees),
      "dec_deg" -> Json.fromDoubleOrNull(input.coordinates.dec.toAngle.toSignedDoubleDegrees),
      "duration_hrs" -> Json.fromDoubleOrNull(input.duration.toHours.toDouble),
      "brightest" -> Json.fromBigDecimal(input.brightest),
      "sp_type" -> input.spType.asJson
    )
  }

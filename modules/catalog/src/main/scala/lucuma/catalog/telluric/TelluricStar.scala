// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.telluric

import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension

/**
 * A telluric standard star candidate from the telluric targets service
 */
case class TelluricStar(
  hip: Int,
  spType: String,
  coordinates: Coordinates,
  distance: Double,
  hmag: Double,
  score: Double,
  order: String
)

object TelluricStar:
  given Decoder[TelluricStar] = Decoder.instance { c =>
    for
      hip      <- c.downField("HIP").as[Int]
      spType   <- c.downField("spType").as[String]
      raDeg    <- c.downField("RA").as[Double]
      decDeg   <- c.downField("Dec").as[Double]
      distance <- c.downField("Distance").as[Double]
      hmag     <- c.downField("Hmag").as[Double]
      score    <- c.downField("Score").as[Double]
      order    <- c.downField("Order").as[String]
      dec      <- Declination.fromDoubleDegrees(decDeg).toRight(
                    DecodingFailure(s"Invalid declination: $decDeg", c.downField("Dec").history)
                  )
      ra       = RightAscension.fromDoubleDegrees(raDeg)
      coords   = Coordinates(ra, dec)
    yield TelluricStar(hip, spType, coords, distance, hmag, score, order)
  }

  given Encoder[TelluricStar] = Encoder.instance { star =>
    Json.obj(
      "hip" -> Json.fromInt(star.hip),
      "spType" -> Json.fromString(star.spType),
      "ra" -> Json.fromDoubleOrNull(star.coordinates.ra.toAngle.toDoubleDegrees),
      "dec" -> Json.fromDoubleOrNull(star.coordinates.dec.toAngle.toSignedDoubleDegrees),
      "distance" -> Json.fromDoubleOrNull(star.distance),
      "hmag" -> Json.fromDoubleOrNull(star.hmag),
      "score" -> Json.fromDoubleOrNull(star.score),
      "order" -> Json.fromString(star.order)
    )
  }

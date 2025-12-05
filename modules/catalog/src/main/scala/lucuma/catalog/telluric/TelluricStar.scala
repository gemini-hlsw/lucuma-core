// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.telluric

import cats.syntax.option.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.catalog.telluric.codecs.given
import lucuma.core.enums.CatalogName
import lucuma.core.enums.TelluricCalibrationOrder
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.RightAscension
import lucuma.core.model.CatalogInfo
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.core.model.TelluricType

import scala.collection.immutable.SortedMap

case class TelluricStar(
  hip:         Int,
  spType:      TelluricType,
  coordinates: Coordinates,
  distance:    Double,
  hmag:        Double,
  score:       Double,
  order:       TelluricCalibrationOrder
):
  val simbadName: NonEmptyString = NonEmptyString.unsafeFrom(s"HIP $hip")

  def asSiderealTarget: Target.Sidereal =
    Target.Sidereal(
      name = simbadName,
      tracking = SiderealTracking(
        baseCoordinates = coordinates,
        epoch = Epoch.J2000,
        properMotion = None,
        radialVelocity = None,
        parallax = None
      ),
      sourceProfile = SourceProfile.Point(
        SpectralDefinition.BandNormalized(None, SortedMap.empty)
      ),
      catalogInfo = CatalogInfo(CatalogName.Telluric, simbadName, none).some
    )

object TelluricStar:
  given Decoder[TelluricStar] = c =>
    for {
      hip      <- c.downField("HIP").as[Int]
      spType   <- c.downField("spType").as[TelluricType]
      raDeg    <- c.downField("RA").as[Double]
      decDeg   <- c.downField("Dec").as[Double]
      distance <- c.downField("Distance").as[Double]
      hmag     <- c.downField("Hmag").as[Double]
      score    <- c.downField("Score").as[Double]
      order    <- c.downField("Order").as[TelluricCalibrationOrder]
      dec      <- Declination
                    .fromDoubleDegrees(decDeg)
                    .toRight(
                      DecodingFailure(s"Invalid declination: $decDeg", c.downField("Dec").history)
                    )
      ra        = RightAscension.fromDoubleDegrees(raDeg)
      coords    = Coordinates(ra, dec)
    } yield TelluricStar(hip, spType, coords, distance, hmag, score, order)

  given Encoder[TelluricStar] = star =>
    Json.obj(
      "hip"      -> Json.fromInt(star.hip),
      "spType"   -> star.spType.asJson,
      "ra"       -> Json.fromDoubleOrNull(star.coordinates.ra.toAngle.toDoubleDegrees),
      "dec"      -> Json.fromDoubleOrNull(star.coordinates.dec.toAngle.toSignedDoubleDegrees),
      "distance" -> Json.fromDoubleOrNull(star.distance),
      "hmag"     -> Json.fromDoubleOrNull(star.hmag),
      "score"    -> Json.fromDoubleOrNull(star.score),
      "order"    -> star.order.asJson
    )

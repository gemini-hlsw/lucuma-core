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
import lucuma.core.enums.StellarLibrarySpectrum
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
import lucuma.core.model.UnnormalizedSED
import lucuma.core.util.Enumerated

import scala.collection.immutable.SortedMap

case class TelluricStar(
  id:          String,
  spType:      TelluricType,
  coordinates: Coordinates,
  distance:    Double,
  hmag:        Double,
  score:       Double,
  order:       TelluricCalibrationOrder,
  sed:         Option[UnnormalizedSED]
):
  val simbadName: NonEmptyString = NonEmptyString.unsafeFrom(id)

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
        SpectralDefinition.BandNormalized(sed, SortedMap.empty)
      ),
      catalogInfo = CatalogInfo(CatalogName.Telluric, simbadName, none).some
    )

object TelluricStar:
  // Can't use the standard encoders as the python server uses a different format
  private def decodeSed(s: String): Option[UnnormalizedSED] =
    Enumerated[StellarLibrarySpectrum]
      .fromTag(s.stripSuffix(".nm"))
      .map(UnnormalizedSED.StellarLibrary.apply)

  private def encodeSed(sed: UnnormalizedSED): Option[Json] =
    sed match
      case UnnormalizedSED.StellarLibrary(s) =>
        Json.fromString(Enumerated[StellarLibrarySpectrum].tag(s)).some
      case _                                 => none

  given Decoder[TelluricStar] = c =>
    for {
      id       <- c.downField("ID").as[String]
      spType   <- c.downField("spType").as[TelluricType]
      raDeg    <- c.downField("RA").as[Double]
      decDeg   <- c.downField("Dec").as[Double]
      distance <- c.downField("Distance").as[Double]
      hmag     <- c.downField("Hmag").as[Double]
      score    <- c.downField("Score").as[Double]
      order    <- c.downField("Order").as[TelluricCalibrationOrder]
      sed       = c.downField("SED").as[String].toOption.flatMap(decodeSed)
      dec      <- Declination
                    .fromDoubleDegrees(decDeg)
                    .toRight(
                      DecodingFailure(s"Invalid declination: $decDeg", c.downField("Dec").history)
                    )
      ra        = RightAscension.fromDoubleDegrees(raDeg)
      coords    = Coordinates(ra, dec)
    } yield TelluricStar(id, spType, coords, distance, hmag, score, order, sed)

  given Encoder[TelluricStar] = star =>
    Json.obj(
      "id"       -> Json.fromString(star.id),
      "spType"   -> star.spType.asJson,
      "ra"       -> Json.fromDoubleOrNull(star.coordinates.ra.toAngle.toDoubleDegrees),
      "dec"      -> Json.fromDoubleOrNull(star.coordinates.dec.toAngle.toSignedDoubleDegrees),
      "distance" -> Json.fromDoubleOrNull(star.distance),
      "hmag"     -> Json.fromDoubleOrNull(star.hmag),
      "score"    -> Json.fromDoubleOrNull(star.score),
      "order"    -> star.order.asJson,
      "sed"      -> star.sed.flatMap(encodeSed).asJson
    )

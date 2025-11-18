// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.telluric

import clue.GraphQLOperation
import clue.annotation.GraphQL
import io.circe.Decoder
import io.circe.DecodingFailure
import lucuma.core.enums.TelluricCalibrationOrder
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.TelluricType
import cats.data.NonEmptyList
import cats.syntax.either.*

@GraphQL
trait TelluricSearchQuery extends GraphQLOperation[TelluricService]:

  val document: String =
    """
        query TelluricSearch($ra_deg: Float!, $dec_deg: Float!, $duration_hrs: Float!, $brightest: Float!, $sp_type: String!) {
          search(ra_deg: $ra_deg, dec_deg: $dec_deg, duration_hrs: $duration_hrs, brightest: $brightest, sp_type: $sp_type) {
            HIP
            spType
            RA
            Dec
            Distance
            Hmag
            Score
            Order
          }
        }
      """

  object Data:
    type Search = TelluricStar

  case class TelluricStar(
    hip:         Int,
    spType:      TelluricType,
    coordinates: Coordinates,
    distance:    Double,
    hmag:        Double,
    score:       Double,
    order:       TelluricCalibrationOrder
  )

  object TelluricStar:
    given Decoder[TelluricType] =
      Decoder[String].emap: s =>
        s.toLowerCase match
          case "hot"   => TelluricType.Hot.asRight
          case "a0v"   => TelluricType.A0V.asRight
          case "solar" => TelluricType.Solar.asRight
          case other   =>
            NonEmptyList.fromList(other.split(",").map(_.trim).filter(_.nonEmpty).toList) match
              case Some(types) => TelluricType.Manual(types).asRight
              case None        => s"Invalid telluric type: $s".asLeft

    given Decoder[TelluricCalibrationOrder] =
      Decoder[String].emap: s =>
        TelluricCalibrationOrder.fromString(s).toRight(s"Invalid telluric calibration order: $s")

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

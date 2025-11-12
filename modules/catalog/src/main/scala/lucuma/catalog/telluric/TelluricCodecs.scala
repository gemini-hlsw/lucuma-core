// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.telluric

import cats.data.NonEmptyList
import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.enums.TelluricCalibrationOrder
import lucuma.core.model.TelluricType

object TelluricCodecs:

  given Decoder[TelluricType] =
    Decoder[String].emap: s =>
      s.toLowerCase match
        case "hot"   => Right(TelluricType.Hot)
        case "a0v"   => Right(TelluricType.A0V)
        case "solar" => Right(TelluricType.Solar)
        case other   =>
          NonEmptyList.fromList(other.split(",").map(_.trim).filter(_.nonEmpty).toList) match
            case Some(types) => Right(TelluricType.Manual(types))
            case None        => Left(s"Invalid telluric type: $s")

  given Encoder[TelluricType] =
    Encoder[String].contramap:
      case TelluricType.Hot               => "hot"
      case TelluricType.A0V               => "A0V"
      case TelluricType.Solar             => "Solar"
      case TelluricType.Manual(starTypes) => starTypes.toList.mkString(",")

  given Decoder[TelluricCalibrationOrder] =
    Decoder[String].emap: s =>
      TelluricCalibrationOrder.fromString(s).toRight(s"Invalid telluric calibration order: $s")

  given Encoder[TelluricCalibrationOrder] =
    Encoder[String].contramap(_.tag)

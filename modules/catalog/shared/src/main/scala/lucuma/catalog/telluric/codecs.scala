// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.telluric

import cats.data.NonEmptyList
import cats.syntax.either.*
import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.enums.TelluricCalibrationOrder
import lucuma.core.model.TelluricType

object codecs:

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

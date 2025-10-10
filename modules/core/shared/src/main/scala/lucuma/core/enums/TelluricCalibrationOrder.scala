// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import io.circe.Decoder
import io.circe.Encoder

/**
 * Telluric calibration order: before or after the observation
 */
enum TelluricCalibrationOrder(val tag: String):
  case Before extends TelluricCalibrationOrder("before")
  case After  extends TelluricCalibrationOrder("after")

object TelluricCalibrationOrder:
  def fromString(s: String): Option[TelluricCalibrationOrder] =
    s.toLowerCase match
      case "before" => Some(Before)
      case "after"  => Some(After)
      case _        => None

  given Decoder[TelluricCalibrationOrder] =
    Decoder[String].emap(s =>
      fromString(s).toRight(s"Invalid telluric calibration order: $s")
    )

  given Encoder[TelluricCalibrationOrder] =
    Encoder[String].contramap(_.tag)

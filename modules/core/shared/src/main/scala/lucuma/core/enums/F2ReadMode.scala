// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.eq.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan


/**
 * Enumerated type for Flamingos2 read modes.
 * @group Enumerations (Generated)
 */
enum F2ReadMode(
  val tag: String,
  val shortName: String,
  val longName: String,
  val description: String,
  val minimumExposureTime: TimeSpan,
  val recommendedExposureTime: TimeSpan,
  val readoutTime: TimeSpan,
  val readCount: Int,
  val readNoise: Double
) derives Enumerated:

  case Bright extends F2ReadMode("Bright", "bright", "Bright Object", "Strong Source", 1500.msTimeSpan, 5000.msTimeSpan, 8000.msTimeSpan, 1, 11.7)
  case Medium extends F2ReadMode("Medium", "medium", "Medium Object", "Medium Source", 6.secTimeSpan,   21.secTimeSpan,  14.secTimeSpan,  4, 6.0)
  case Faint  extends F2ReadMode("Faint",  "faint",  "Faint Object",  "Weak Source",   12.secTimeSpan,  85.secTimeSpan,  20.secTimeSpan,  8, 5.0)


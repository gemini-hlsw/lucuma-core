// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.order.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan


/**
 * Enumerated type for Flamingos2 read modes.
 * @group Enumerations
 */
enum Flamingos2ReadMode(
  val tag: String,
  val shortName: String,
  val longName: String,
  val description: String,
  val minimumExposureTime: TimeSpan,
  val recommendedExposureTime: TimeSpan,
  val readoutTime: TimeSpan,
  val readCount: Flamingos2Reads,
  val readNoise: Double
) derives Enumerated:

  case Bright extends Flamingos2ReadMode("Bright", "bright", "Bright Object", "Strong Source", 1500.msTimeSpan, 5000.msTimeSpan, 8000.msTimeSpan, Flamingos2Reads.Reads_1, 11.7)
  case Medium extends Flamingos2ReadMode("Medium", "medium", "Medium Object", "Medium Source", 6.secTimeSpan,   21.secTimeSpan,  14.secTimeSpan,  Flamingos2Reads.Reads_4, 6.0)
  case Faint  extends Flamingos2ReadMode("Faint",  "faint",  "Faint Object",  "Weak Source",   12.secTimeSpan,  85.secTimeSpan,  20.secTimeSpan,  Flamingos2Reads.Reads_8, 5.0)

object Flamingos2ReadMode:

  def forExposureTime(t: TimeSpan): Flamingos2ReadMode =
    if t >= Faint.recommendedExposureTime then Faint
    else if t >= Medium.recommendedExposureTime then Medium
    else Bright
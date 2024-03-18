// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import lucuma.core.arb.ArbTime
import lucuma.core.math.JulianDate
import org.scalacheck.Arbitrary.*
import org.scalacheck.*

import java.time.LocalDateTime

trait ArbJulianDate {
  import ArbTime.given

  given Arbitrary[JulianDate] =
    Arbitrary {
      arbitrary[LocalDateTime].map(JulianDate.ofLocalDateTime)
    }

  given Cogen[JulianDate] =
    Cogen[(Int, Long)].contramap(jd => (jd.dayNumber, jd.nanoAdjustment))
}

object ArbJulianDate extends ArbJulianDate

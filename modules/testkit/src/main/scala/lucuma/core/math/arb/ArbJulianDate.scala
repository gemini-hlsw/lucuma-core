// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import lucuma.core.math.JulianDate

import java.time.LocalDateTime

import org.scalacheck._
import org.scalacheck.Arbitrary._
import lucuma.core.arb.ArbTime

trait ArbJulianDate {
  import ArbTime._

  implicit val arbJulianDate: Arbitrary[JulianDate] =
    Arbitrary {
      arbitrary[LocalDateTime].map(JulianDate.ofLocalDateTime)
    }

  implicit val cogJulianDate: Cogen[JulianDate] =
    Cogen[(Int, Long)].contramap(jd => (jd.dayNumber, jd.nanoAdjustment))
}

object ArbJulianDate extends ArbJulianDate

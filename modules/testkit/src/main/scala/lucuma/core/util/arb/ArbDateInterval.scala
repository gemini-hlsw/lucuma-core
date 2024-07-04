// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util
package arb

import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

import java.time.LocalDate

trait ArbDateInterval {

  given Arbitrary[DateInterval] =
    Arbitrary {
      for {
        d0 <- arbitrary[LocalDate]
        d1 <- arbitrary[LocalDate]
      } yield DateInterval.between(d0, d1)
    }

  given Cogen[DateInterval] =
    Cogen[(LocalDate, LocalDate)].contramap { a => (a.start, a.end) }

}

object ArbDateInterval extends ArbDateInterval
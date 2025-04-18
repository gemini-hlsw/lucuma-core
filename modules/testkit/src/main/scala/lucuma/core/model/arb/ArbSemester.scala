// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import lucuma.core.enums.Half
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

trait ArbSemester {
  import ArbEnumerated.given

  given Arbitrary[Semester.YearInt] =
    Arbitrary {
      Gen
        .choose(Semester.YearInt.MinValue.value, Semester.YearInt.MaxValue.value)
        .map(Semester.YearInt.unsafeFrom)
    }

  given Cogen[Semester.YearInt] =
    Cogen[Int].contramap(_.value)

  given Arbitrary[Semester] =
    Arbitrary {
      for {
        year <- arbitrary[Semester.YearInt]
        half <- arbitrary[Half]
      } yield Semester(year, half)
    }

  given Cogen[Semester] =
    Cogen[(Semester.YearInt, Half)].contramap(s => (s.yearInt, s.half))

}

object ArbSemester extends ArbSemester

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package model
package arb

import lucuma.core.arb.ArbTime
import lucuma.core.enums.Half
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary._
import org.scalacheck._

import java.time.Year

trait ArbSemester {
  import ArbEnumerated._
  import ArbTime._

  implicit val arbSemester: Arbitrary[Semester] =
    Arbitrary {
      for {
        year <- arbitrary[Year]
        half <- arbitrary[Half]
      } yield Semester(year, half)
    }

  implicit val cogSemester: Cogen[Semester] =
    Cogen[(Year, Half)].contramap(s => (s.year, s.half))

}

object ArbSemester extends ArbSemester

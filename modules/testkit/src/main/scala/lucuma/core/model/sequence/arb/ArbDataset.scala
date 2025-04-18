// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.Site
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

import java.time.LocalDate
import scala.util.control.Exception.allCatch


trait ArbDataset {
  import ArbEnumerated.given

  given Arbitrary[Dataset.Filename] =
    Arbitrary {
      for {
        s <- arbitrary[Site]
        y <- Gen.choose[Int](0, 9999)
        m <- Gen.oneOf(java.time.Month.values().toList)
        d <- Gen.choose[Int](1, 31).filter(d => allCatch.opt(LocalDate.of(y, m, d)).isDefined)
        i <- arbitrary[PosInt]
      } yield Dataset.Filename.from(s, LocalDate.of(y, m, d), i).get
    }

  given Cogen[Dataset.Filename] =
    Cogen[(Site, LocalDate, PosInt)].contramap { a => (
      a.site,
      a.localDate,
      a.index
    )}
}

object ArbDataset extends ArbDataset

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen


trait ArbCategorizedTimeRange {

  import ArbCategorizedTime.given

  given Arbitrary[CategorizedTimeRange] =
    Arbitrary {
      for {
        p0 <- arbitrary[CategorizedTime]
        p1 <- arbitrary[CategorizedTime]
      } yield CategorizedTimeRange.from(p0, p1)
    }

  given Cogen[CategorizedTimeRange] =
    Cogen[(CategorizedTime, CategorizedTime)].contramap(a => (a.min, a.max))

}

object ArbCategorizedTimeRange extends ArbCategorizedTimeRange
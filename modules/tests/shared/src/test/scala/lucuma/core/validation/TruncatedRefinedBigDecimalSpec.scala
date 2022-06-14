// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.validation

import cats.kernel.laws.discipline.OrderTests
import eu.timepit.refined.cats._
import lucuma.core.optics.laws.discipline.SplitEpiTests
import lucuma.core.validation.arb._
import munit.DisciplineSuite
import org.scalacheck.Arbitrary._

class TruncatedRefinedBigDecimalSpec extends DisciplineSuite {
  import ArbTruncatedRefinedBigDecimal._

  checkAll("TruncatedRefinedBigDecimal",
           OrderTests[TruncatedRefinedBigDecimal[OneToThree, 1]].order
  )

  checkAll("TruncatedRefinedBigDecimal.unsafeRefinedBigDecimal",
           SplitEpiTests(TruncatedRefinedBigDecimal.unsafeRefinedBigDecimal[OneToThree, 1]).splitEpi
  )
}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.validation

import cats.kernel.laws.discipline.EqTests
import lucuma.core.math.arb._
import lucuma.core.optics.laws.discipline.SplitEpiTests
import lucuma.core.validation.arb._
import munit.DisciplineSuite
import org.scalacheck.Arbitrary._

class TruncatedRASpec extends DisciplineSuite {
  import ArbRightAscension._
  import ArbTruncatedRA._

  // Laws
  checkAll("TruncatedRA", EqTests[TruncatedRA].eqv)

  // Optics
  checkAll("TruncatedRA.rightAscension", SplitEpiTests(TruncatedRA.rightAscension).splitEpi)
}

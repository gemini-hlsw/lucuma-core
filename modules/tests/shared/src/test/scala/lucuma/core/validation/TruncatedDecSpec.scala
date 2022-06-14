// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.validation

import cats.kernel.laws.discipline.EqTests
import lucuma.core.math.arb._
import lucuma.core.optics.laws.discipline.SplitEpiTests
import lucuma.core.validation.arb._
import munit.DisciplineSuite
import org.scalacheck.Arbitrary._

class TruncatedDecSpec extends DisciplineSuite {
  import ArbDeclination._
  import ArbTruncatedDec._

  // Laws
  checkAll("TruncatedDec", EqTests[TruncatedDec].eqv)

  // Optics
  checkAll("TruncatedDec.declination", SplitEpiTests(TruncatedDec.declination).splitEpi)
}

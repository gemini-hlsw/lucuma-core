// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.implicits._
import cats.kernel.laws.discipline._
import lucuma.core.math.arb.ArbProperVelocity._
import munit.FunSuite
import munit.DisciplineSuite
import munit.ScalaCheckSuite

final class ProperVelocitySuite extends FunSuite with DisciplineSuite with ScalaCheckSuite {

  // Laws
  checkAll("Order[ProperVelocity]", OrderTests[ProperVelocity].order)
  checkAll("Monoid[ProperVelocity]", MonoidTests[ProperVelocity].monoid)
}

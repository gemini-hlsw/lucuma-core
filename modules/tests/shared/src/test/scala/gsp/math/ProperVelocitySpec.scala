// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats.tests.CatsSuite
import cats.kernel.laws.discipline._
import gsp.math.arb.ArbProperVelocity._

final class ProperVelocitySpec extends CatsSuite {

  // Laws
  checkAll("Order[ProperVelocity]", OrderTests[ProperVelocity].order)
  checkAll("Monoid[ProperVelocity]", MonoidTests[ProperVelocity].monoid)
}

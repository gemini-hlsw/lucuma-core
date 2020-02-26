// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats.tests.CatsSuite
import gsp.math.laws.discipline._
import gsp.math.arb._
import monocle.law.discipline._

final class RadialVelocitySpec extends CatsSuite {
  import ArbRadialVelocity._

  // Laws
  checkAll("RadialVelocity.fromMetersPerSecond", PrismTests(RadialVelocity.fromMetersPerSecond))
  checkAll("RadialVelocity.fromKilometersPerSecond", FormatTests(RadialVelocity.fromKilometersPerSecond).format)

}
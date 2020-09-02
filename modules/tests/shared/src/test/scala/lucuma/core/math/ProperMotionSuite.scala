// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.kernel.laws.discipline._
import lucuma.core.math.arb._
import monocle.law.discipline._
import munit.DisciplineSuite
import org.scalacheck.Prop.forAll

final class ProperMotionSuite extends DisciplineSuite {
  import ArbParallax._
  import ArbCoordinates._
  import ArbEpoch._
  import ArbProperVelocity._
  import ArbProperMotion._
  import ArbRadialVelocity._

  // Laws
  checkAll("ProperMotion", OrderTests[ProperMotion].order)
  checkAll("ProperMotion.baseCoordinates", LensTests(ProperMotion.baseCoordinates))
  checkAll("ProperMotion.epoch", LensTests(ProperMotion.epoch))
  checkAll("ProperMotion.properVelocity", LensTests(ProperMotion.properVelocity))
  checkAll("ProperMotion.radialVelocity", LensTests(ProperMotion.radialVelocity))
  checkAll("ProperMotion.parallax", LensTests(ProperMotion.parallax))

  test("ProperMotion.identity") {
    forAll { (pm: ProperMotion) =>
      val c1 = pm.baseCoordinates
      val c2 = pm.plusYears(0.0).baseCoordinates
      assert(c1.angularDistance(c2).toMicroarcseconds <= 20L)
    }
  }

}

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import lucuma.core.math.arb._
import lucuma.core.model.arb._
import monocle.law.discipline._
import munit.DisciplineSuite
import org.scalacheck.Prop.forAll

final class SiderealTrackingSuite extends DisciplineSuite {
  import ArbParallax._
  import ArbCoordinates._
  import ArbEpoch._
  import ArbProperMotion._
  import ArbSiderealTracking._
  import ArbRadialVelocity._

  // Laws
  checkAll("SiderealTracking", OrderTests[SiderealTracking].order)
  checkAll("SiderealTracking.baseCoordinates", LensTests(SiderealTracking.baseCoordinates))
  checkAll("SiderealTracking.epoch", LensTests(SiderealTracking.epoch))
  checkAll("SiderealTracking.properMotion", LensTests(SiderealTracking.properMotion))
  checkAll("SiderealTracking.radialVelocity", LensTests(SiderealTracking.radialVelocity))
  checkAll("SiderealTracking.parallax", LensTests(SiderealTracking.parallax))

  test("SiderealTracking.identity") {
    forAll { (pm: SiderealTracking) =>
      val c1 = pm.baseCoordinates
      val c2 = pm.plusYears(0.0)
      assert(c1.angularDistance(c2).toMicroarcseconds <= 20L)
    }
  }

}

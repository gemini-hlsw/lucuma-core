// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import lucuma.core.math.arb._
import lucuma.core.model.arb._
import monocle.law.discipline._
import munit.DisciplineSuite
import org.scalacheck.Prop.forAll

final class SiderealTrackingSuite extends DisciplineSuite {
  import ArbCoordinates._
  import ArbDeclination._
  import ArbEpoch._
  import ArbParallax._
  import ArbProperMotion._
  import ArbRightAscension._
  import ArbSiderealTracking._
  import ArbRadialVelocity._

  // Laws
  checkAll("SiderealTracking", OrderTests[SiderealTracking].order)
  checkAll("SiderealTracking.baseCoordinates", LensTests(SiderealTracking.baseCoordinates))
  checkAll("SiderealTracking.baseRa", LensTests(SiderealTracking.baseRa))
  checkAll("SiderealTracking.baseDec", LensTests(SiderealTracking.baseDec))
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

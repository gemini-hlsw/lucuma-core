// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import cats.syntax.all._
import lucuma.core.math.Coordinates
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.arb._
import lucuma.core.model.arb._
import lucuma.core.syntax.instant.*
import lucuma.core.util.Timestamp
import monocle.law.discipline._
import munit.DisciplineSuite
import org.scalacheck.Prop.forAll

import java.time.Instant

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
      assert(c2.exists(c2 => c1.angularDistance(c2).toMicroarcseconds <= 20L))
    }
  }

  test("coordinatesOn corrected by cos(dec) case 1") {
    val coord = Coordinates.fromHmsDms.getOption("11 05 28.577 +43 31 36.39").get
    val pmra = ProperMotion.RA.milliarcsecondsPerYear.reverseGet(BigDecimal(-4406.469))
    val pmdec = ProperMotion.Dec.milliarcsecondsPerYear.reverseGet(BigDecimal(938.527))
    val tracking = SiderealTracking(coord, Epoch.J2000, ProperMotion(pmra, pmdec).some, none, Parallax.fromMicroarcseconds(203887).some)
    val refEpoch = Instant.ofEpochSecond(4102444800L)
    assertEquals(tracking.at(refEpoch), Coordinates.fromHmsDms.getOption("11 04 48.043284 +43 33 09.795210"))
  }

  test("coordinatesOn corrected by cos(dec) case 2") {
    val coord = Coordinates.fromHmsDms.getOption("14 29 42.946 -62 40 46.16").get
    val pmra = ProperMotion.RA.milliarcsecondsPerYear.reverseGet(BigDecimal(-3781.741))
    val pmdec = ProperMotion.Dec.milliarcsecondsPerYear.reverseGet(BigDecimal(769.465))
    val tracking = SiderealTracking(coord, Epoch.J2000, ProperMotion(pmra, pmdec).some, none, Parallax.fromMicroarcseconds(768465).some)
    val refEpoch = Instant.ofEpochSecond(4102444800L)
    assertEquals(tracking.at(refEpoch), Coordinates.fromHmsDms.getOption("14 28 48.054809 -62 39 28.543030"))
  }

}

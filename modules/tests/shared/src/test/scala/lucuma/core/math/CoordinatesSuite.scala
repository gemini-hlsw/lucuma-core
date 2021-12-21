// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Eq
import cats.Show
import cats.kernel.laws.discipline._
import lucuma.core.math.arb._
import lucuma.core.optics.laws.discipline._
import monocle.law.discipline._
import org.scalacheck.Prop._

final class CoordinatesSuite extends munit.DisciplineSuite {
  import ArbCoordinates._
  import ArbRightAscension._
  import ArbDeclination._
  import ArbAngle._

  // Laws
  checkAll("Coordinates", OrderTests[Coordinates].order)
  checkAll("Coordinates.fromHmsDms",
           FormatTests(Coordinates.fromHmsDms).formatWith(ArbCoordinates.strings)
  )
  checkAll("Coordinates.rightAscension", LensTests(Coordinates.rightAscension))
  checkAll("Coordinates.declination", LensTests(Coordinates.declination))

  test("Equality must be natural") {
    forAll { (a: Coordinates, b: Coordinates) =>
      assertEquals(a.equals(b), Eq[Coordinates].eqv(a, b))
    }
  }

  test("Show must be natural") {
    forAll { (a: Coordinates) =>
      assertEquals(a.toString, Show[Coordinates].show(a))
    }
  }

  test("offsetWithCarry must be consistent with offset") {
    forAll { (a: Coordinates, dRA: HourAngle, dDec: Angle) =>
      assertEquals(a.offset(dRA, dDec), a.offsetWithCarry(dRA, dDec)._1)
    }
  }

  test("offsetWithCarry must be invertable") {
    forAll { (a: Coordinates, dRA: HourAngle, dDec: Angle) =>
      a.offsetWithCarry(dRA, dDec) match {
        case (cs, false) => assertEquals(cs.offset(-dRA, -dDec), a)
        case (cs, true)  => assertEquals(cs.offset(-dRA, dDec), a)
      }
    }
  }

  test("diff must be consistent with offset") {
    forAll { (a: Coordinates, b: Coordinates) =>
      val (dRA, dDec) = a.diff(b)
      assertEquals(a.offset(dRA, dDec), b)
    }
  }

  test("diff must be consistent with offsetWithCarry, and never carry") {
    forAll { (a: Coordinates, b: Coordinates) =>
      val (dRA, dDec) = a.diff(b)
      assertEquals(a.offsetWithCarry(dRA, dDec), (b, false))
    }
  }

  test("angularDistance must be in [0, 180°]") {
    forAll { (a: Coordinates, b: Coordinates) =>
      assert(a.angularDistance(b).toMicroarcseconds <= Angle.Angle180.toMicroarcseconds)
    }
  }

  test("angularDistance must be zero between any point and itself") {
    forAll { (c: Coordinates) =>
      assertEquals(c.angularDistance(c), Angle.Angle0)
    }
  }

  test("angularDistance must be symmetric to within 1µas") {
    forAll { (a: Coordinates, b: Coordinates) =>
      val Δ = a.angularDistance(b) - b.angularDistance(a)
      assert(Angle.signedMicroarcseconds.get(Δ).abs <= 1L)
    }
  }

  test("angularDistance must be exactly 180° between the poles, regardless of RA") {
    forAll { (ra1: RA, ra2: RA) =>
      val s = Coordinates(ra1, Dec.Min)
      val n = Coordinates(ra2, Dec.Max)
      assertEquals(n.angularDistance(s), Angle.Angle180)
    }
  }

  test(
    "angularDistance must be 90° between either pole and any point on the equator, regardless of RA, within 1µas"
  ) {
    forAll { (ra1: RA, ra2: RA, b: Boolean) =>
      val pole  = Coordinates(ra1, if (b) Dec.Min else Dec.Max)
      val point = Coordinates(ra2, Dec.Zero)
      val delta = point.angularDistance(pole)
      assert((delta.toMicroarcseconds - Angle.Angle90.toMicroarcseconds).abs <= 1L)
    }
  }

  test(
    "angularDistance must equal any offset in declination from either pole, regardless of RA, to within 1µas"
  ) {
    forAll { (ra1: RA, ra2: RA, dec: Dec, b: Boolean) =>
      val pole = Coordinates(ra1, if (b) Dec.Min else Dec.Max)
      val Δdec = dec.toAngle + Angle.Angle90 // [0, 180]
      val Δ    = pole.angularDistance(pole.offset(ra2.toHourAngle, Δdec)) - Δdec
      assert(Angle.signedMicroarcseconds.get(Δ).abs <= 1L)
    }
  }

  test(
    "angularDistance must equal any offset in right ascension along the equator, to within 1µas"
  ) {
    forAll { (ra: RA, ha: HourAngle) =>
      val a = Coordinates(ra, Dec.Zero)
      val b = a.offset(ha, Angle.Angle0)
      val d = a.angularDistance(b)
      val Δ = Angle.signedMicroarcseconds.get(d).abs - Angle.signedMicroarcseconds.get(ha).abs
      assert(Δ.abs <= 1L)
    }
  }

  test(
    "interpolate should result in angular distance of 0° from `a` for factor 0.0, within 1µsec (15µas)"
  ) {
    forAll { (a: Coordinates, b: Coordinates) =>
      val Δ = a.angularDistance(a.interpolate(b, 0.0))
      assert(Angle.signedMicroarcseconds.get(Δ).abs <= 15L)
    }
  }

  test(
    "interpolate should result in angular distance of 0° from `b` for factor 1.0, within 1µsec (15µas)"
  ) {
    forAll { (a: Coordinates, b: Coordinates) =>
      val Δ = b.angularDistance(a.interpolate(b, 1.0))
      assert(Angle.signedMicroarcseconds.get(Δ).abs <= 15L)
    }
  }

  test("interpolate should be consistent with fractional angular separation, to within 20 µas") {
    val µas180 = Angle.Angle180.toMicroarcseconds
    val µas360 = µas180 * 2L

    forAll { (c1: Coordinates, c2: Coordinates) =>
      val sep = c1.angularDistance(c2)
      val Δs  = (Range.BigDecimal(-1.0, 2.0, 0.1)).map { f =>
        val stepSep  = c1.interpolate(c2, f.toDouble).angularDistance(c1).toMicroarcseconds
        val fracSep  = (sep.toMicroarcseconds * f.abs).toLong
        val fracSepʹ = if (fracSep <= µas180) fracSep else µas360 - fracSep
        (stepSep - fracSepʹ).abs
      }
      assert(Δs.filter(_ > 20L).isEmpty)
    }

  }

}

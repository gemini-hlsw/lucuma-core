// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Eq
import cats.Show
import cats.data.NonEmptyList
import cats.kernel.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import cats.syntax.all.*
import lucuma.core.math.Coordinates.centerOf
import lucuma.core.math.arb.*
import lucuma.core.optics.laws.discipline.*
import lucuma.core.tests.ScalaCheckFlaky
import monocle.law.discipline.*
import org.scalacheck.Prop.*

final class CoordinatesSuite extends munit.DisciplineSuite {
  import ArbCoordinates.*
  import ArbRightAscension.*
  import ArbDeclination.*
  import ArbAngle.*
  import ArbOffset.*

  // Laws
  checkAll("Coordinates", OrderTests[Coordinates].order)
  checkAll(
    "Coordinates.fromHmsDms",
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
      val Δ    = pole.angularDistance(pole.shift(ra2.toHourAngle, Δdec)) - Δdec
      assert(Angle.signedMicroarcseconds.get(Δ).abs <= 1L)
    }
  }

  test(
    "angularDistance must equal any offset in right ascension along the equator, to within 1µas"
  ) {
    forAll { (ra: RA, ha: HourAngle) =>
      val a = Coordinates(ra, Dec.Zero)
      val b = a.shift(ha, Angle.Angle0)
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

  test(
    "interpolate should be consistent with fractional angular separation, to within 20 µas"
      .tag(ScalaCheckFlaky)
  ) {
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

  test("offsetBy 0 is identical") {
    forAll { (a: Coordinates, posAngle: Angle) =>
      assertEquals(a.offsetBy(posAngle, Offset.Zero), a.some)
    }
  }

  test("offsetBy complements itself (with a small error)") {
    forAll { (a: Coordinates, posAngle: Angle, offset: Offset) =>
      val b = a.offsetBy(posAngle, offset)
      val c = b.flatMap(_.offsetBy(posAngle, -offset))
      assertEqualsDouble((b, c).mapN(_.angularDistance(_).toDoubleDegrees).getOrElse(Double.MaxValue), 0, 0.01)
    }
  }

  def assertCoordsEquals(a: Coordinates, b: Coordinates) = {
    assertEqualsDouble(a.ra.toRadians, b.ra.toRadians, 1e-10)
    assertEqualsDouble(a.dec.toRadians, b.dec.toRadians, 1e-10)
  }

  test("center of same is same") {
    forAll { (coord: Coordinates) =>
      assertCoordsEquals(centerOf(NonEmptyList.of(coord, coord)), coord)
    }
  }

  test("centerOf is commutative") {
    forAll { (a: Coordinates, b: Coordinates) =>
      assertCoordsEquals(centerOf(NonEmptyList.of(a, b)), centerOf(NonEmptyList.of(b, a)))
    }
  }

  test("centerOf is approximately interpolate") {
    forAll { (a: Coordinates, b: Coordinates) =>
      assertCoordsEquals(centerOf(NonEmptyList.of(a, b)), a.interpolate(b, 0.5))
      assertCoordsEquals(centerOf(NonEmptyList.of(a, b)), b.interpolate(a, 0.5))
    }
  }

  test("centerOf should be independent of order") {
    forAll { (coords: NonEmptyList[Coordinates]) =>
      assertCoordsEquals(centerOf(coords), centerOf(coords.reverse))
      assertCoordsEquals(centerOf(coords),
                         centerOf(NonEmptyList.ofInitLast(coords.tail, coords.head))
      )
    }
  }

}

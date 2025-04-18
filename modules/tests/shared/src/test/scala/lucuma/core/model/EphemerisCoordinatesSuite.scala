// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Show
import cats.kernel.laws.discipline.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbCoordinates
import lucuma.core.math.arb.ArbDeclination
import lucuma.core.math.arb.ArbOffset
import lucuma.core.math.arb.ArbRightAscension
import lucuma.core.model.arb.ArbEphemeris
import monocle.law.discipline.*
import munit.*
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.*

final class EphemerisCoordinatesSuite extends DisciplineSuite {
  import ArbCoordinates.given
  import ArbEphemeris.given
  import ArbOffset.given
  import ArbRightAscension.given
  import ArbDeclination.given
  import EphemerisCoordinatesSuite.*

  // Laws
  checkAll("EphemerisCoordinates", EqTests[EphemerisCoordinates].eqv)
  checkAll("EphemerisCoordinates.coordinates", LensTests(EphemerisCoordinates.coordinates))
  checkAll("EphemerisCoordinates.rightAscension", LensTests(EphemerisCoordinates.rightAscension))
  checkAll("EphemerisCoordinates.declination", LensTests(EphemerisCoordinates.declination))
  checkAll("EphemerisCoordinates.delta", LensTests(EphemerisCoordinates.delta))
  checkAll("EphemerisCoordinates.deltaP", LensTests(EphemerisCoordinates.deltaP))
  checkAll("EphemerisCoordinates.deltaQ", LensTests(EphemerisCoordinates.deltaQ))

  test("Equality must be natural") {
    forAll { (a: EphemerisCoordinates, b: EphemerisCoordinates) =>
      assertEquals(a.equals(b), Eq[EphemerisCoordinates].eqv(a, b))
    }
  }

  test("Show must be natural") {
    forAll { (a: EphemerisCoordinates) =>
      assertEquals(a.toString, Show[EphemerisCoordinates].show(a))
    }
  }

  test("interpolate must be consistent with Coordinates.interpolate") {
    forAll { (a: EphemerisCoordinates, b: EphemerisCoordinates, r: Ratio) =>
      assertEquals(a.interpolate(b, r.ratio).coord, a.coord.interpolate(b.coord, r.ratio))
    }
  }

  test("interpolate velocity between any point and itself must yield the same velocity") {
    forAll { (a: EphemerisCoordinates, r: Ratio) =>
      assertEquals(a.interpolate(a, r.ratio).delta, a.delta)
    }
  }

  test("interpolate velocity with factor 0 should yield the first point") {
    forAll { (a: EphemerisCoordinates, b: EphemerisCoordinates) =>
      assertEquals(a.interpolate(b, 0.0).delta, a.delta)
    }
  }

  test("interpolate velocity with factor 1 should yield the second point") {
    forAll { (a: EphemerisCoordinates, b: EphemerisCoordinates) =>
      assertEquals(a.interpolate(b, 1.0).delta, b.delta)
    }
  }

  private def midpoint(
    a: EphemerisCoordinates,
    b: EphemerisCoordinates,
    f: Offset => Angle
  ): Unit = {
    val m  = a.interpolate(b, 0.5).delta
    val mΔ = Angle.signedMicroarcseconds.get(f(m))

    val aΔ = Angle.signedMicroarcseconds.get(f(a.delta))
    val bΔ = Angle.signedMicroarcseconds.get(f(b.delta))

    assertEquals(mΔ, ((aΔ + bΔ) / 2.0).round)
  }

  test("interpolate velocity with factor 0.5 should yield the midpoint p") {
    forAll { (a: EphemerisCoordinates, b: EphemerisCoordinates) =>
      midpoint(a, b, _.p.toAngle)
    }
  }

  test("interpolate velocity with factor 0.5 should yield the midpoint q") {
    forAll { (a: EphemerisCoordinates, b: EphemerisCoordinates) =>
      midpoint(a, b, _.q.toAngle)
    }
  }

}

object EphemerisCoordinatesSuite {

  final case class Ratio(ratio: Double)

  implicit val arbRatio: Arbitrary[Ratio] =
    Arbitrary {
      Gen.choose(0.0, 1.0).map(Ratio(_))
    }

}

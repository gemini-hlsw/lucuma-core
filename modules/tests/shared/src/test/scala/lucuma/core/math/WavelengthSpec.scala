// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.tests.CatsSuite
import cats.{ Eq, Order, Show }
import cats.kernel.laws.discipline._
import lucuma.core.math.arb._
import lucuma.core.math.units._
import monocle.law.discipline._
import coulomb.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.numeric._
import eu.timepit.refined.scalacheck.numeric._
import eu.timepit.refined.types.numeric.PosInt

final class WavelengthSpec extends CatsSuite {
  import ArbWavelength._

  // Laws
  checkAll("Wavelength", OrderTests[Wavelength].order)
  checkAll("picometers", IsoTests(Wavelength.picometers))
  checkAll("fromPicometers", PrismTests(Wavelength.fromPicometers))

  test("Equality must be natural") {
    forAll { (a: Wavelength, b: Wavelength) =>
      a.equals(b) shouldEqual Eq[Wavelength].eqv(a, b)
    }
  }

  test("Order must be consistent with .toPicometers") {
    forAll { (a: Wavelength, b: Wavelength) =>
      Order[PosInt].comparison(a.toPicometers.value, b.toPicometers.value) shouldEqual
        Order[Wavelength].comparison(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: Wavelength) =>
      a.toString shouldEqual Show[Wavelength].show(a)
    }
  }

  test("Conversions") {
    Wavelength.fromAngstrom(1) shouldEqual Wavelength(100.withRefinedUnit[Positive, Picometer]).some
    Wavelength.fromNanometers(1) shouldEqual Wavelength(
      1000.withRefinedUnit[Positive, Picometer]
    ).some
    Wavelength.fromNanometers(Wavelength.MaxNanometer + 1) shouldEqual none
    Wavelength.fromAngstrom(Wavelength.MaxAngstrom + 1) shouldEqual none
  }

  test("toAngstrom") {
    forAll { (a: PosInt) =>
      whenever(a.value <= Wavelength.MaxAngstrom) {
        Wavelength.fromAngstrom(a).map(_.angstrom.value.isWhole) shouldEqual true.some
        Wavelength.fromAngstrom(a).map(_.angstrom.value) shouldEqual a.value.some
      }
    }
  }

  test("toNanometers") {
    forAll { (a: PosInt) =>
      whenever(a.value <= Wavelength.MaxNanometer) {
        Wavelength.fromNanometers(a).map(_.angstrom.value.isWhole) shouldEqual true.some
        Wavelength.fromNanometers(a).map(_.nm.value) shouldEqual a.value.some
      }
    }
  }
}

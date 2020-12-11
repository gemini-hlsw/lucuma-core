// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.tests.CatsSuite
import cats.{Eq, Order, Show}
import cats.kernel.laws.discipline._
import coulomb.Quantity
import lucuma.core.math.arb._
import lucuma.core.math.units._
import monocle.law.discipline._
import coulomb.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.numeric._
import eu.timepit.refined.scalacheck.numeric._
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.optics.Format
import lucuma.core.optics.laws.discipline.FormatTests
import org.scalatest.Assertion
import spire.math.Rational

import java.math.RoundingMode

final class WavelengthSpec extends CatsSuite {
  import ArbWavelength._

  // Laws
  checkAll("Wavelength", OrderTests[Wavelength].order)
  checkAll("picometers", IsoTests(Wavelength.picometers))
  checkAll("fromPicometers", PrismTests(Wavelength.fromPicometers))

  checkAll("decimalPicometers",   FormatTests(Wavelength.decimalPicometers).formatWith(bigDecimalWavelengths))
  checkAll("decimalAngstroms",    FormatTests(Wavelength.decimalAngstroms).formatWith(bigDecimalWavelengths))
  checkAll("decimalNanometers",   FormatTests(Wavelength.decimalNanometers).formatWith(bigDecimalWavelengths))
  checkAll("decimalMicrometers",  FormatTests(Wavelength.decimalMicrometers).formatWith(bigDecimalWavelengths))

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
    Wavelength.fromAngstroms(1) shouldEqual Wavelength(100.withRefinedUnit[Positive, Picometer]).some
    Wavelength.fromNanometers(1) shouldEqual Wavelength(
      1000.withRefinedUnit[Positive, Picometer]
    ).some
    Wavelength.fromMicrometers(1) shouldEqual Wavelength(
      1000000.withRefinedUnit[Positive, Picometer]
    ).some

    Wavelength.decimalPicometers.getOption(BigDecimal("1.4")) shouldEqual Wavelength.fromPicometers.getOption(1)
    Wavelength.decimalPicometers.getOption(BigDecimal("1.5")) shouldEqual Wavelength.fromPicometers.getOption(2)
    Wavelength.decimalPicometers.getOption(BigDecimal("2147483647.0")) shouldEqual Wavelength.Max.some
    Wavelength.decimalPicometers.getOption(BigDecimal("2147483647.5")) shouldEqual none

    Wavelength.decimalAngstroms.getOption(BigDecimal("1.5")) shouldEqual Wavelength.fromPicometers.getOption(150)
    Wavelength.decimalAngstroms.getOption(BigDecimal("1.004")) shouldEqual Wavelength.fromPicometers.getOption(100)
    Wavelength.decimalAngstroms.getOption(BigDecimal("1.005")) shouldEqual Wavelength.fromPicometers.getOption(101)
    Wavelength.decimalAngstroms.getOption(BigDecimal("21474836.47")) shouldEqual Wavelength.Max.some
    Wavelength.decimalAngstroms.getOption(BigDecimal("21474836.475")) shouldEqual none

    Wavelength.decimalNanometers.getOption(BigDecimal("1.5")) shouldEqual Wavelength.fromPicometers.getOption(1500)
    Wavelength.decimalNanometers.getOption(BigDecimal("1.0004")) shouldEqual Wavelength.fromPicometers.getOption(1000)
    Wavelength.decimalNanometers.getOption(BigDecimal("1.0005")) shouldEqual Wavelength.fromPicometers.getOption(1001)
    Wavelength.decimalNanometers.getOption(BigDecimal("2147483.647")) shouldEqual Wavelength.Max.some
    Wavelength.decimalNanometers.getOption(BigDecimal("2147483.6475")) shouldEqual none

    Wavelength.decimalMicrometers.getOption(BigDecimal("1.5")) shouldEqual Wavelength.fromPicometers.getOption(1500000)
    Wavelength.decimalMicrometers.getOption(BigDecimal("1.0000004")) shouldEqual Wavelength.fromPicometers.getOption(1000000)
    Wavelength.decimalMicrometers.getOption(BigDecimal("1.0000005")) shouldEqual Wavelength.fromPicometers.getOption(1000001)
    Wavelength.decimalMicrometers.getOption(BigDecimal("2147.483647")) shouldEqual Wavelength.Max.some
    Wavelength.decimalMicrometers.getOption(BigDecimal("2147.4836475")) shouldEqual none

    Wavelength.fromMicrometers(Wavelength.MaxMicrometer + 1) shouldEqual none
    Wavelength.fromNanometers(Wavelength.MaxNanometer + 1) shouldEqual none
    Wavelength.fromAngstroms(Wavelength.MaxAngstrom + 1) shouldEqual none
  }

  private def testFormat[U](scale: Int, format: Format[BigDecimal, Wavelength])(toRational: Wavelength => Quantity[Rational, U]): Assertion =
    forAll { (w: Wavelength) =>
      format.getOption(toRational(w).value.toBigDecimal(scale, RoundingMode.HALF_UP)) shouldEqual w.some
    }

  test("picometer decimal format") {
    testFormat(1, Wavelength.decimalPicometers)(_.toPicometers.to[Rational, Picometer])
  }

  test("angstrom decimal format") {
    testFormat(3, Wavelength.decimalAngstroms)(_.angstrom)
  }

  test("nanometers decimal format") {
    testFormat(4, Wavelength.decimalNanometers)(_.nanometer)
  }

  test("micrometers decimal format") {
    testFormat(7, Wavelength.decimalMicrometers)(_.micrometer)
  }

  test("toAngstrom") {
    forAll { (a: PosInt) =>
      whenever(a.value <= Wavelength.MaxAngstrom) {
        Wavelength.fromAngstroms(a).map(_.angstrom.value.isWhole) shouldEqual true.some
        Wavelength.fromAngstroms(a).map(_.angstrom.value) shouldEqual a.value.some
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

  test("toMicrons") {
    forAll { (a: PosInt) =>
      whenever(a.value <= Wavelength.MaxMicrometer) {
        Wavelength.fromMicrometers(a).map(_.angstrom.value.isWhole) shouldEqual true.some
        Wavelength.fromMicrometers(a).map(_.nm.value.isWhole) shouldEqual true.some
        Wavelength.fromMicrometers(a).map(_.µm.value) shouldEqual a.value.some
      }
    }
  }
}

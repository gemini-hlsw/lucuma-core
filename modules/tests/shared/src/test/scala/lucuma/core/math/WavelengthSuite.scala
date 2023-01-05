// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Eq
import cats.Order
import cats.Show
import cats.kernel.laws.discipline._
import cats.syntax.all._
import coulomb.*
import coulomb.policy.spire.standard.given
import coulomb.syntax.*
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.numeric._
import eu.timepit.refined.scalacheck.numeric._
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.arb._
import lucuma.core.math.units.{_, given}
import lucuma.core.optics.Format
import lucuma.core.optics.laws.discipline.FormatTests
import monocle.law.discipline._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import spire.math.Rational

import java.math.RoundingMode

final class WavelengthSuite extends munit.DisciplineSuite {
  import ArbWavelength._

  // Override to remove implicit modifier
  override def unitToProp = super.unitToProp
  // Scala 3 likes this better
  implicit def saneUnitToProp(unit: Unit): Prop = unitToProp(unit)

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
      assertEquals(a.equals(b),  Eq[Wavelength].eqv(a, b))
    }
  }

  test("Order must be consistent with .toPicometers") {
    forAll { (a: Wavelength, b: Wavelength) =>
      assertEquals(Order[PosInt].comparison(a.toPicometers.value, b.toPicometers.value),
        Order[Wavelength].comparison(a, b))
    }
  }

  test("Show must be natural") {
    forAll { (a: Wavelength) =>
      assertEquals(a.toString,  Show[Wavelength].show(a))
    }
  }

  test("Conversions") {
    assertEquals(Wavelength.fromAngstroms(1),  Wavelength(100.withRefinedUnit[Positive, Picometer]).some)
    assertEquals(Wavelength.fromNanometers(1),  Wavelength(
      1000.withRefinedUnit[Positive, Picometer]
    ).some)
    assertEquals(Wavelength.fromMicrometers(1),  Wavelength(
      1000000.withRefinedUnit[Positive, Picometer]
    ).some)

    assertEquals(Wavelength.decimalPicometers.getOption(BigDecimal("1.4")),  Wavelength.fromPicometers.getOption(1))
    assertEquals(Wavelength.decimalPicometers.getOption(BigDecimal("1.5")),  Wavelength.fromPicometers.getOption(2))
    assertEquals(Wavelength.decimalPicometers.getOption(BigDecimal("2147483647.0")),  Wavelength.Max.some)
    assertEquals(Wavelength.decimalPicometers.getOption(BigDecimal("2147483647.5")),  none)

    assertEquals(Wavelength.decimalAngstroms.getOption(BigDecimal("1.5")),  Wavelength.fromPicometers.getOption(150))
    assertEquals(Wavelength.decimalAngstroms.getOption(BigDecimal("1.004")),  Wavelength.fromPicometers.getOption(100))
    assertEquals(Wavelength.decimalAngstroms.getOption(BigDecimal("1.005")),  Wavelength.fromPicometers.getOption(101))
    assertEquals(Wavelength.decimalAngstroms.getOption(BigDecimal("21474836.47")),  Wavelength.Max.some)
    assertEquals(Wavelength.decimalAngstroms.getOption(BigDecimal("21474836.475")),  none)

    assertEquals(Wavelength.decimalNanometers.getOption(BigDecimal("1.5")),  Wavelength.fromPicometers.getOption(1500))
    assertEquals(Wavelength.decimalNanometers.getOption(BigDecimal("1.0004")),  Wavelength.fromPicometers.getOption(1000))
    assertEquals(Wavelength.decimalNanometers.getOption(BigDecimal("1.0005")),  Wavelength.fromPicometers.getOption(1001))
    assertEquals(Wavelength.decimalNanometers.getOption(BigDecimal("2147483.647")),  Wavelength.Max.some)
    assertEquals(Wavelength.decimalNanometers.getOption(BigDecimal("2147483.6475")),  none)

    assertEquals(Wavelength.decimalMicrometers.getOption(BigDecimal("1.5")),  Wavelength.fromPicometers.getOption(1500000))
    assertEquals(Wavelength.decimalMicrometers.getOption(BigDecimal("1.0000004")),  Wavelength.fromPicometers.getOption(1000000))
    assertEquals(Wavelength.decimalMicrometers.getOption(BigDecimal("1.0000005")),  Wavelength.fromPicometers.getOption(1000001))
    assertEquals(Wavelength.decimalMicrometers.getOption(BigDecimal("2147.483647")),  Wavelength.Max.some)
    assertEquals(Wavelength.decimalMicrometers.getOption(BigDecimal("2147.4836475")),  none)

    assertEquals(Wavelength.fromMicrometers(Wavelength.MaxMicrometer + 1),  none)
    assertEquals(Wavelength.fromNanometers(Wavelength.MaxNanometer + 1),  none)
    assertEquals(Wavelength.fromAngstroms(Wavelength.MaxAngstrom + 1),  none)
  }

  private def testFormat[U](format: Format[BigDecimal, Wavelength])(toPosBigDecimal: Wavelength => Quantity[PosBigDecimal, U]) =
    forAll { (w: Wavelength) =>
      assertEquals(
        format.getOption(toPosBigDecimal(w).value.value),
        w.some
      )
    }

  test("picometer decimal format") {
    testFormat(Wavelength.decimalPicometers) { w =>
      Quantity[Picometer](PosBigDecimal.unsafeFrom(BigDecimal(w.toPicometers.value.value)))
    }
  }

  test("angstrom decimal format") {
    testFormat(Wavelength.decimalAngstroms)(_.toAngstroms)
  }

  test("nanometers decimal format") {
    testFormat(Wavelength.decimalNanometers)(_.toNanometers)
  }

  test("micrometers decimal format") {
    testFormat(Wavelength.decimalMicrometers)(_.toMicrometers)
  }

  test("toAngstrom") {
    forAll { (a: PosInt) =>
      (a.value <= Wavelength.MaxAngstrom) ==> {
        assertEquals(Wavelength.fromAngstroms(a).map(_.toAngstroms.value.isWhole),  true.some)
        assertEquals(Wavelength.fromAngstroms(a).map(_.toAngstroms.value), PosBigDecimal.unsafeFrom(a.value).some)
      }
    }
  }

  test("toNanometers") {
    forAll { (a: PosInt) =>
      (a.value <= Wavelength.MaxNanometer) ==> {
        assertEquals(Wavelength.fromNanometers(a).map(_.toAngstroms.value.isWhole),  true.some)
        assertEquals(Wavelength.fromNanometers(a).map(_.toNanometers.value), PosBigDecimal.unsafeFrom(a.value).some)
      }
    }
  }

  test("toMicrons") {
    forAll { (a: PosInt) =>
      (a.value <= Wavelength.MaxMicrometer) ==> {
        assertEquals(Wavelength.fromMicrometers(a).map(_.toAngstroms.value.isWhole),  true.some)
        assertEquals(Wavelength.fromMicrometers(a).map(_.toNanometers.value.isWhole),  true.some)
        assertEquals(Wavelength.fromMicrometers(a).map(_.toMicrometers.value), PosBigDecimal.unsafeFrom(a.value).some)
      }
    }
  }
}

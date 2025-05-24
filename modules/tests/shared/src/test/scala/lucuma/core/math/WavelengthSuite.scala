// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Eq
import cats.Order
import cats.Show
import cats.kernel.laws.discipline.*
import cats.syntax.all.*
import coulomb.*
import coulomb.ops.algebra.cats.quantity.given
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.numeric.*
import eu.timepit.refined.scalacheck.numeric.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.arb.*
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.math.units.*
import lucuma.core.optics.Format
import lucuma.core.optics.laws.discipline.FormatTests
import monocle.law.discipline.*
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop.*

final class WavelengthSuite extends munit.DisciplineSuite {
  import ArbQuantity.given
  import ArbRefined.given
  import ArbWavelength.given
  import ArbWavelengthDither.given

  given Conversion[Unit, Prop] = unitToProp(_)

  // Laws
  checkAll("Wavelength", OrderTests[Wavelength].order)

  checkAll("picometers",  IsoTests(Wavelength.picometers))
  checkAll("angstroms",   FormatTests(Wavelength.angstroms).formatLaws)
  checkAll("nanometers",  FormatTests(Wavelength.nanometers).formatLaws)
  checkAll("micrometers", FormatTests(Wavelength.micrometers).formatLaws)

  checkAll("intPicometers",      PrismTests(Wavelength.intPicometers))
  checkAll("decimalPicometers",  FormatTests(Wavelength.decimalPicometers).formatWith(ArbWavelength.bigDecimalWavelengths))
  checkAll("decimalAngstroms",   FormatTests(Wavelength.decimalAngstroms).formatWith(ArbWavelength.bigDecimalWavelengths))
  checkAll("decimalNanometers",  FormatTests(Wavelength.decimalNanometers).formatWith(ArbWavelength.bigDecimalWavelengths))
  checkAll("decimalMicrometers", FormatTests(Wavelength.decimalMicrometers).formatWith(ArbWavelength.bigDecimalWavelengths))

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
    assertEquals(Wavelength.fromIntAngstroms(1),   Wavelength(      100.withRefinedUnit[Positive, Picometer]).some)
    assertEquals(Wavelength.fromIntNanometers(1),  Wavelength(    1_000.withRefinedUnit[Positive, Picometer]).some)
    assertEquals(Wavelength.fromIntMicrometers(1), Wavelength(1_000_000.withRefinedUnit[Positive, Picometer]).some)

    assertEquals(Wavelength.decimalPicometers.getOption(BigDecimal("1.4")),           Wavelength.intPicometers.getOption(1))
    assertEquals(Wavelength.decimalPicometers.getOption(BigDecimal("1.5")),           Wavelength.intPicometers.getOption(2))
    assertEquals(Wavelength.decimalPicometers.getOption(BigDecimal("2147483647.0")),  Wavelength.Max.some)
    assertEquals(Wavelength.decimalPicometers.getOption(BigDecimal("2147483647.5")),  none)

    assertEquals(Wavelength.decimalAngstroms.getOption(BigDecimal("1.5")),          Wavelength.intPicometers.getOption(150))
    assertEquals(Wavelength.decimalAngstroms.getOption(BigDecimal("1.004")),        Wavelength.intPicometers.getOption(100))
    assertEquals(Wavelength.decimalAngstroms.getOption(BigDecimal("1.005")),        Wavelength.intPicometers.getOption(101))
    assertEquals(Wavelength.decimalAngstroms.getOption(BigDecimal("21474836.47")),  Wavelength.Max.some)
    assertEquals(Wavelength.decimalAngstroms.getOption(BigDecimal("21474836.475")), none)

    assertEquals(Wavelength.decimalNanometers.getOption(BigDecimal("1.5")),          Wavelength.intPicometers.getOption(1500))
    assertEquals(Wavelength.decimalNanometers.getOption(BigDecimal("1.0004")),       Wavelength.intPicometers.getOption(1000))
    assertEquals(Wavelength.decimalNanometers.getOption(BigDecimal("1.0005")),       Wavelength.intPicometers.getOption(1001))
    assertEquals(Wavelength.decimalNanometers.getOption(BigDecimal("2147483.647")),  Wavelength.Max.some)
    assertEquals(Wavelength.decimalNanometers.getOption(BigDecimal("2147483.6475")), none)

    assertEquals(Wavelength.decimalMicrometers.getOption(BigDecimal("1.5")),          Wavelength.intPicometers.getOption(1_500_000))
    assertEquals(Wavelength.decimalMicrometers.getOption(BigDecimal("1.0000004")),    Wavelength.intPicometers.getOption(1_000_000))
    assertEquals(Wavelength.decimalMicrometers.getOption(BigDecimal("1.0000005")),    Wavelength.intPicometers.getOption(1_000_001))
    assertEquals(Wavelength.decimalMicrometers.getOption(BigDecimal("2147.483647")),  Wavelength.Max.some)
    assertEquals(Wavelength.decimalMicrometers.getOption(BigDecimal("2147.4836475")), none)

    assertEquals(Wavelength.fromIntMicrometers(Wavelength.MaxMicrometer + 1), none)
    assertEquals(Wavelength.fromIntNanometers(Wavelength.MaxNanometer + 1),   none)
    assertEquals(Wavelength.fromIntAngstroms(Wavelength.MaxAngstrom + 1),     none)
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
      (a <= Wavelength.MaxAngstrom) ==> {
        assertEquals(Wavelength.fromIntAngstroms(a.value).map(_.toAngstroms.value.isWhole),  true.some)
        assertEquals(Wavelength.fromIntAngstroms(a.value).map(_.toAngstroms.value), PosBigDecimal.unsafeFrom(a.value).some)
      }
    }
  }

  test("toNanometers") {
    forAll { (a: PosInt) =>
      (a.value <= Wavelength.MaxNanometer) ==> {
        assertEquals(Wavelength.fromIntNanometers(a.value).map(_.toAngstroms.value.isWhole),  true.some)
        assertEquals(Wavelength.fromIntNanometers(a.value).map(_.toNanometers.value), PosBigDecimal.unsafeFrom(a.value).some)
      }
    }
  }

  test("toMicrons") {
    forAll { (a: PosInt) =>
      (a.value <= Wavelength.MaxMicrometer) ==> {
        assertEquals(Wavelength.fromIntMicrometers(a.value).map(_.toAngstroms.value.isWhole),  true.some)
        assertEquals(Wavelength.fromIntMicrometers(a.value).map(_.toNanometers.value.isWhole),  true.some)
        assertEquals(Wavelength.fromIntMicrometers(a.value).map(_.toMicrometers.value), PosBigDecimal.unsafeFrom(a.value).some)
      }
    }
  }

  property("offset") {
    forAll { (w: Wavelength, d: WavelengthDither) =>
      assertEquals(
        w.offset(d),
        Wavelength.intPicometers.getOption(w.toPicometers.value.value + d.toPicometers.value)
      )
    }
  }

  test("difference") {
    forAll { (a: Wavelength, b: Wavelength) =>
      assertEquals(a.diff(a), WavelengthDither.Zero)
      assertEquals(a.diff(b).abs, b.diff(a).abs)
    }
  }

}

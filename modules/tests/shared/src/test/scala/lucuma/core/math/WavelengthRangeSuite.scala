// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Eq
import cats.Order
import cats.Show
import cats.kernel.laws.discipline.*
import cats.syntax.all.*
import coulomb.*
import coulomb.ops.algebra.cats.quantity.given
import coulomb.syntax.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.numeric.*
import eu.timepit.refined.scalacheck.numeric.*
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.arb.*
import lucuma.core.math.units.{_, given}
import lucuma.core.optics.Format
import lucuma.core.optics.laws.discipline.FormatTests
import monocle.law.discipline.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop.*
import spire.math.Rational

import java.math.RoundingMode

final class WavelengthRangeSuite extends munit.DisciplineSuite {
  import ArbQuantity.given
  import ArbRefined.given
  import ArbWavelength.*
  import ArbWavelengthRange.given

  given Conversion[Unit, Prop] = unitToProp(_)

  // Laws
  checkAll("WavelengthRange", OrderTests[WavelengthRange].order)

  checkAll("picometers",  IsoTests(WavelengthRange.picometers))
  checkAll("angstroms",   FormatTests(WavelengthRange.angstroms).format)
  checkAll("nanometers",  FormatTests(WavelengthRange.nanometers).format)
  checkAll("micrometers", FormatTests(WavelengthRange.micrometers).format)

  checkAll("intPicometers",      PrismTests(WavelengthRange.intPicometers))
  checkAll("decimalPicometers",  FormatTests(WavelengthRange.decimalPicometers).formatWith(bigDecimalWavelengths))
  checkAll("decimalAngstroms",   FormatTests(WavelengthRange.decimalAngstroms).formatWith(bigDecimalWavelengths))
  checkAll("decimalNanometers",  FormatTests(WavelengthRange.decimalNanometers).formatWith(bigDecimalWavelengths))
  checkAll("decimalMicrometers", FormatTests(WavelengthRange.decimalMicrometers).formatWith(bigDecimalWavelengths))

  test("Equality must be natural") {
    forAll { (a: WavelengthRange, b: WavelengthRange) =>
      assertEquals(a.equals(b),  Eq[WavelengthRange].eqv(a, b))
    }
  }

  test("Order must be consistent with .toPicometers") {
    forAll { (a: WavelengthRange, b: WavelengthRange) =>
      assertEquals(Order[PosInt].comparison(a.toPicometers.value, b.toPicometers.value),
        Order[WavelengthRange].comparison(a, b))
    }
  }

  test("Show must be natural") {
    forAll { (a: WavelengthRange) =>
      assertEquals(a.toString,  Show[WavelengthRange].show(a))
    }
  }

  test("Conversions") {
    assertEquals(WavelengthRange.fromIntAngstroms(1),   WavelengthRange(      100.withRefinedUnit[Positive, Picometer]).some)
    assertEquals(WavelengthRange.fromIntNanometers(1),  WavelengthRange(    1_000.withRefinedUnit[Positive, Picometer]).some)
    assertEquals(WavelengthRange.fromIntMicrometers(1), WavelengthRange(1_000_000.withRefinedUnit[Positive, Picometer]).some)

    assertEquals(WavelengthRange.decimalPicometers.getOption(BigDecimal("1.4")),           WavelengthRange.intPicometers.getOption(1))
    assertEquals(WavelengthRange.decimalPicometers.getOption(BigDecimal("1.5")),           WavelengthRange.intPicometers.getOption(2))
    assertEquals(WavelengthRange.decimalPicometers.getOption(BigDecimal("2147483647.5")),  none)

    assertEquals(WavelengthRange.decimalAngstroms.getOption(BigDecimal("1.5")),          WavelengthRange.intPicometers.getOption(150))
    assertEquals(WavelengthRange.decimalAngstroms.getOption(BigDecimal("1.004")),        WavelengthRange.intPicometers.getOption(100))
    assertEquals(WavelengthRange.decimalAngstroms.getOption(BigDecimal("1.005")),        WavelengthRange.intPicometers.getOption(101))
    assertEquals(WavelengthRange.decimalAngstroms.getOption(BigDecimal("21474836.475")), none)

    assertEquals(WavelengthRange.decimalNanometers.getOption(BigDecimal("1.5")),          WavelengthRange.intPicometers.getOption(1500))
    assertEquals(WavelengthRange.decimalNanometers.getOption(BigDecimal("1.0004")),       WavelengthRange.intPicometers.getOption(1000))
    assertEquals(WavelengthRange.decimalNanometers.getOption(BigDecimal("1.0005")),       WavelengthRange.intPicometers.getOption(1001))
    assertEquals(WavelengthRange.decimalNanometers.getOption(BigDecimal("2147483.6475")), none)

    assertEquals(WavelengthRange.decimalMicrometers.getOption(BigDecimal("1.5")),          WavelengthRange.intPicometers.getOption(1_500_000))
    assertEquals(WavelengthRange.decimalMicrometers.getOption(BigDecimal("1.0000004")),    WavelengthRange.intPicometers.getOption(1_000_000))
    assertEquals(WavelengthRange.decimalMicrometers.getOption(BigDecimal("1.0000005")),    WavelengthRange.intPicometers.getOption(1_000_001))
    assertEquals(WavelengthRange.decimalMicrometers.getOption(BigDecimal("2147.4836475")), none)

    assertEquals(WavelengthRange.fromIntMicrometers(Wavelength.MaxMicrometer + 1), none)
    assertEquals(WavelengthRange.fromIntNanometers(Wavelength.MaxNanometer + 1),   none)
    assertEquals(WavelengthRange.fromIntAngstroms(Wavelength.MaxAngstrom + 1),     none)
  }

  private def testFormat[U](format: Format[BigDecimal, WavelengthRange])(toPosBigDecimal: WavelengthRange => Quantity[PosBigDecimal, U]) =
    forAll { (w: WavelengthRange) =>
      assertEquals(
        format.getOption(toPosBigDecimal(w).value.value),
        w.some
      )
    }

  test("picometer decimal format") {
    testFormat(WavelengthRange.decimalPicometers) { w =>
      Quantity[Picometer](PosBigDecimal.unsafeFrom(BigDecimal(w.toPicometers.value.value)))
    }
  }

  test("angstrom decimal format") {
    testFormat(WavelengthRange.decimalAngstroms)(_.toAngstroms)
  }

  test("nanometers decimal format") {
    testFormat(WavelengthRange.decimalNanometers)(_.toNanometers)
  }

  test("micrometers decimal format") {
    testFormat(WavelengthRange.decimalMicrometers)(_.toMicrometers)
  }

  test("toAngstrom") {
    forAll { (a: PosInt) =>
      (a.value <= Wavelength.MaxAngstrom) ==> {
        assertEquals(WavelengthRange.fromIntAngstroms(a.value).map(_.toAngstroms.value.isWhole), true.some)
        assertEquals(WavelengthRange.fromIntAngstroms(a.value).map(_.toAngstroms.value), PosBigDecimal.unsafeFrom(a.value).some)
      }
    }
  }

  test("toNanometers") {
    forAll { (a: PosInt) =>
      (a.value <= Wavelength.MaxNanometer) ==> {
        assertEquals(WavelengthRange.fromIntNanometers(a.value).map(_.toAngstroms.value.isWhole), true.some)
        assertEquals(WavelengthRange.fromIntNanometers(a.value).map(_.toNanometers.value), PosBigDecimal.unsafeFrom(a.value).some)
      }
    }
  }

  test("toMicrons") {
    forAll { (a: PosInt) =>
      (a.value <= Wavelength.MaxMicrometer) ==> {
        assertEquals(WavelengthRange.fromIntMicrometers(a.value).map(_.toAngstroms.value.isWhole), true.some)
        assertEquals(WavelengthRange.fromIntMicrometers(a.value).map(_.toNanometers.value.isWhole), true.some)
        assertEquals(WavelengthRange.fromIntMicrometers(a.value).map(_.toMicrometers.value), PosBigDecimal.unsafeFrom(a.value).some)
      }
    }
  }

  test("centeredAt") {
    forAll{ (wc: WavelengthRange, w: Wavelength) =>
      val range = wc.centeredAt(w)
      val halfDown = math.floor(wc.pm.value / 2.0).toInt
      val halfUp = math.ceil(wc.pm.value / 2.0).toInt
      assert(range.upper.pm.value.value - range.lower.pm.value.value <= wc.pm.value.value)
      (halfDown <= w.pm.value && w.pm.value <= (Wavelength.Max.pm.value - halfUp)) ==> {
        assertEquals(range.upper.pm.value.value - range.lower.pm.value.value, wc.pm.value.value)
      }
    }
  }

  test("startingAt") {
    forAll{ (wc: WavelengthRange, w: Wavelength) =>
      val range = wc.startingAt(w)
      assert(range.upper.pm.value.value - range.lower.pm.value.value <= wc.pm.value.value)
      (w.pm.value <= (Wavelength.Max.pm.value - wc.pm.value)) ==> {
        assertEquals(range.upper.pm.value.value - range.lower.pm.value.value, wc.pm.value.value)
      }
    }
  }

  test("endingAt") {
    forAll{ (wc: WavelengthRange, w: Wavelength) =>
      val range = wc.endingAt(w)
      assert(range.upper.pm.value.value - range.lower.pm.value.value <= wc.pm.value.value)
      (wc.pm < w.pm) ==> {
        assertEquals(range.upper.pm.value.value - range.lower.pm.value.value, wc.pm.value.value)
      }
    }
  }

}

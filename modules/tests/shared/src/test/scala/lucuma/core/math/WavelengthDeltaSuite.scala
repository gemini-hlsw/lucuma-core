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
import lucuma.core.math.units.*
import lucuma.core.optics.Format
import lucuma.core.optics.laws.discipline.FormatTests
import monocle.law.discipline.*
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop.*

final class WavelengthDeltaSuite extends munit.DisciplineSuite {
  import ArbQuantity.given
  import ArbRefined.given
  import ArbWavelength.given
  import ArbWavelengthDelta.given

  given Conversion[Unit, Prop] = unitToProp(_)

  // Laws
  checkAll("WavelengthDelta", OrderTests[WavelengthDelta].order)

  checkAll("picometers",  IsoTests(WavelengthDelta.picometers))
  checkAll("angstroms",   FormatTests(WavelengthDelta.angstroms).format)
  checkAll("nanometers",  FormatTests(WavelengthDelta.nanometers).format)
  checkAll("micrometers", FormatTests(WavelengthDelta.micrometers).format)

  checkAll("intPicometers",      PrismTests(WavelengthDelta.intPicometers))
  checkAll("decimalPicometers",  FormatTests(WavelengthDelta.decimalPicometers).formatWith(ArbWavelength.bigDecimalWavelengths))
  checkAll("decimalAngstroms",   FormatTests(WavelengthDelta.decimalAngstroms).formatWith(ArbWavelength.bigDecimalWavelengths))
  checkAll("decimalNanometers",  FormatTests(WavelengthDelta.decimalNanometers).formatWith(ArbWavelength.bigDecimalWavelengths))
  checkAll("decimalMicrometers", FormatTests(WavelengthDelta.decimalMicrometers).formatWith(ArbWavelength.bigDecimalWavelengths))

  test("Equality must be natural") {
    forAll { (a: WavelengthDelta, b: WavelengthDelta) =>
      assertEquals(a.equals(b),  Eq[WavelengthDelta].eqv(a, b))
    }
  }

  test("Order must be consistent with .toPicometers") {
    forAll { (a: WavelengthDelta, b: WavelengthDelta) =>
      assertEquals(Order[PosInt].comparison(a.toPicometers.value, b.toPicometers.value),
        Order[WavelengthDelta].comparison(a, b))
    }
  }

  test("Show must be natural") {
    forAll { (a: WavelengthDelta) =>
      assertEquals(a.toString,  Show[WavelengthDelta].show(a))
    }
  }

  test("Conversions") {
    assertEquals(WavelengthDelta.fromIntAngstroms(1),   WavelengthDelta(      100.withRefinedUnit[Positive, Picometer]).some)
    assertEquals(WavelengthDelta.fromIntNanometers(1),  WavelengthDelta(    1_000.withRefinedUnit[Positive, Picometer]).some)
    assertEquals(WavelengthDelta.fromIntMicrometers(1), WavelengthDelta(1_000_000.withRefinedUnit[Positive, Picometer]).some)

    assertEquals(WavelengthDelta.decimalPicometers.getOption(BigDecimal("1.4")),           WavelengthDelta.intPicometers.getOption(1))
    assertEquals(WavelengthDelta.decimalPicometers.getOption(BigDecimal("1.5")),           WavelengthDelta.intPicometers.getOption(2))
    assertEquals(WavelengthDelta.decimalPicometers.getOption(BigDecimal("2147483647.5")),  none)

    assertEquals(WavelengthDelta.decimalAngstroms.getOption(BigDecimal("1.5")),          WavelengthDelta.intPicometers.getOption(150))
    assertEquals(WavelengthDelta.decimalAngstroms.getOption(BigDecimal("1.004")),        WavelengthDelta.intPicometers.getOption(100))
    assertEquals(WavelengthDelta.decimalAngstroms.getOption(BigDecimal("1.005")),        WavelengthDelta.intPicometers.getOption(101))
    assertEquals(WavelengthDelta.decimalAngstroms.getOption(BigDecimal("21474836.475")), none)

    assertEquals(WavelengthDelta.decimalNanometers.getOption(BigDecimal("1.5")),          WavelengthDelta.intPicometers.getOption(1500))
    assertEquals(WavelengthDelta.decimalNanometers.getOption(BigDecimal("1.0004")),       WavelengthDelta.intPicometers.getOption(1000))
    assertEquals(WavelengthDelta.decimalNanometers.getOption(BigDecimal("1.0005")),       WavelengthDelta.intPicometers.getOption(1001))
    assertEquals(WavelengthDelta.decimalNanometers.getOption(BigDecimal("2147483.6475")), none)

    assertEquals(WavelengthDelta.decimalMicrometers.getOption(BigDecimal("1.5")),          WavelengthDelta.intPicometers.getOption(1_500_000))
    assertEquals(WavelengthDelta.decimalMicrometers.getOption(BigDecimal("1.0000004")),    WavelengthDelta.intPicometers.getOption(1_000_000))
    assertEquals(WavelengthDelta.decimalMicrometers.getOption(BigDecimal("1.0000005")),    WavelengthDelta.intPicometers.getOption(1_000_001))
    assertEquals(WavelengthDelta.decimalMicrometers.getOption(BigDecimal("2147.4836475")), none)

    assertEquals(WavelengthDelta.fromIntMicrometers(Wavelength.MaxMicrometer + 1), none)
    assertEquals(WavelengthDelta.fromIntNanometers(Wavelength.MaxNanometer + 1),   none)
    assertEquals(WavelengthDelta.fromIntAngstroms(Wavelength.MaxAngstrom + 1),     none)
  }

  private def testFormat[U](format: Format[BigDecimal, WavelengthDelta])(toPosBigDecimal: WavelengthDelta => Quantity[PosBigDecimal, U]) =
    forAll { (w: WavelengthDelta) =>
      assertEquals(
        format.getOption(toPosBigDecimal(w).value.value),
        w.some
      )
    }

  test("picometer decimal format") {
    testFormat(WavelengthDelta.decimalPicometers) { w =>
      Quantity[Picometer](PosBigDecimal.unsafeFrom(BigDecimal(w.toPicometers.value.value)))
    }
  }

  test("angstrom decimal format") {
    testFormat(WavelengthDelta.decimalAngstroms)(_.toAngstroms)
  }

  test("nanometers decimal format") {
    testFormat(WavelengthDelta.decimalNanometers)(_.toNanometers)
  }

  test("micrometers decimal format") {
    testFormat(WavelengthDelta.decimalMicrometers)(_.toMicrometers)
  }

  test("toAngstrom") {
    forAll { (a: PosInt) =>
      (a.value <= Wavelength.MaxAngstrom) ==> {
        assertEquals(WavelengthDelta.fromIntAngstroms(a.value).map(_.toAngstroms.value.isWhole), true.some)
        assertEquals(WavelengthDelta.fromIntAngstroms(a.value).map(_.toAngstroms.value), PosBigDecimal.unsafeFrom(a.value).some)
      }
    }
  }

  test("toNanometers") {
    forAll { (a: PosInt) =>
      (a.value <= Wavelength.MaxNanometer) ==> {
        assertEquals(WavelengthDelta.fromIntNanometers(a.value).map(_.toAngstroms.value.isWhole), true.some)
        assertEquals(WavelengthDelta.fromIntNanometers(a.value).map(_.toNanometers.value), PosBigDecimal.unsafeFrom(a.value).some)
      }
    }
  }

  test("toMicrons") {
    forAll { (a: PosInt) =>
      (a.value <= Wavelength.MaxMicrometer) ==> {
        assertEquals(WavelengthDelta.fromIntMicrometers(a.value).map(_.toAngstroms.value.isWhole), true.some)
        assertEquals(WavelengthDelta.fromIntMicrometers(a.value).map(_.toNanometers.value.isWhole), true.some)
        assertEquals(WavelengthDelta.fromIntMicrometers(a.value).map(_.toMicrometers.value), PosBigDecimal.unsafeFrom(a.value).some)
      }
    }
  }

  test("centeredAt") {
    forAll{ (wc: WavelengthDelta, w: Wavelength) =>
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
    forAll{ (wc: WavelengthDelta, w: Wavelength) =>
      val range = wc.startingAt(w)
      assert(range.upper.pm.value.value - range.lower.pm.value.value <= wc.pm.value.value)
      (w.pm.value <= (Wavelength.Max.pm.value - wc.pm.value)) ==> {
        assertEquals(range.upper.pm.value.value - range.lower.pm.value.value, wc.pm.value.value)
      }
    }
  }

  test("endingAt") {
    forAll{ (wc: WavelengthDelta, w: Wavelength) =>
      val range = wc.endingAt(w)
      assert(range.upper.pm.value.value - range.lower.pm.value.value <= wc.pm.value.value)
      (wc.pm < w.pm) ==> {
        assertEquals(range.upper.pm.value.value - range.lower.pm.value.value, wc.pm.value.value)
      }
    }
  }

}

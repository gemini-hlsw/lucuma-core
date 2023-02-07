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

final class WavelengthCoverageSuite extends munit.DisciplineSuite {
  import ArbQuantity.given
  import ArbRefined.*
  import ArbWavelength.*
  import ArbWavelengthCoverage.given

  given Conversion[Unit, Prop] = unitToProp(_)

  // Laws
  checkAll("WavelengthCoverage", OrderTests[WavelengthCoverage].order)

  checkAll("picometers",  IsoTests(WavelengthCoverage.picometers))
  checkAll("angstroms",   FormatTests(WavelengthCoverage.angstroms).format)
  checkAll("nanometers",  FormatTests(WavelengthCoverage.nanometers).format)
  checkAll("micrometers", FormatTests(WavelengthCoverage.micrometers).format)

  checkAll("intPicometers",      PrismTests(WavelengthCoverage.intPicometers))
  checkAll("decimalPicometers",  FormatTests(WavelengthCoverage.decimalPicometers).formatWith(bigDecimalWavelengths))
  checkAll("decimalAngstroms",   FormatTests(WavelengthCoverage.decimalAngstroms).formatWith(bigDecimalWavelengths))
  checkAll("decimalNanometers",  FormatTests(WavelengthCoverage.decimalNanometers).formatWith(bigDecimalWavelengths))
  checkAll("decimalMicrometers", FormatTests(WavelengthCoverage.decimalMicrometers).formatWith(bigDecimalWavelengths))

  test("Equality must be natural") {
    forAll { (a: WavelengthCoverage, b: WavelengthCoverage) =>
      assertEquals(a.equals(b),  Eq[WavelengthCoverage].eqv(a, b))
    }
  }

  test("Order must be consistent with .toPicometers") {
    forAll { (a: WavelengthCoverage, b: WavelengthCoverage) =>
      assertEquals(Order[PosInt].comparison(a.toPicometers.value, b.toPicometers.value),
        Order[WavelengthCoverage].comparison(a, b))
    }
  }

  test("Show must be natural") {
    forAll { (a: WavelengthCoverage) =>
      assertEquals(a.toString,  Show[WavelengthCoverage].show(a))
    }
  }

  test("Conversions") {
    assertEquals(WavelengthCoverage.fromIntAngstroms(1),   WavelengthCoverage(      100.withRefinedUnit[Positive, Picometer]).some)
    assertEquals(WavelengthCoverage.fromIntNanometers(1),  WavelengthCoverage(    1_000.withRefinedUnit[Positive, Picometer]).some)
    assertEquals(WavelengthCoverage.fromIntMicrometers(1), WavelengthCoverage(1_000_000.withRefinedUnit[Positive, Picometer]).some)

    assertEquals(WavelengthCoverage.decimalPicometers.getOption(BigDecimal("1.4")),           WavelengthCoverage.intPicometers.getOption(1))
    assertEquals(WavelengthCoverage.decimalPicometers.getOption(BigDecimal("1.5")),           WavelengthCoverage.intPicometers.getOption(2))
    assertEquals(WavelengthCoverage.decimalPicometers.getOption(BigDecimal("2147483647.5")),  none)

    assertEquals(WavelengthCoverage.decimalAngstroms.getOption(BigDecimal("1.5")),          WavelengthCoverage.intPicometers.getOption(150))
    assertEquals(WavelengthCoverage.decimalAngstroms.getOption(BigDecimal("1.004")),        WavelengthCoverage.intPicometers.getOption(100))
    assertEquals(WavelengthCoverage.decimalAngstroms.getOption(BigDecimal("1.005")),        WavelengthCoverage.intPicometers.getOption(101))
    assertEquals(WavelengthCoverage.decimalAngstroms.getOption(BigDecimal("21474836.475")), none)

    assertEquals(WavelengthCoverage.decimalNanometers.getOption(BigDecimal("1.5")),          WavelengthCoverage.intPicometers.getOption(1500))
    assertEquals(WavelengthCoverage.decimalNanometers.getOption(BigDecimal("1.0004")),       WavelengthCoverage.intPicometers.getOption(1000))
    assertEquals(WavelengthCoverage.decimalNanometers.getOption(BigDecimal("1.0005")),       WavelengthCoverage.intPicometers.getOption(1001))
    assertEquals(WavelengthCoverage.decimalNanometers.getOption(BigDecimal("2147483.6475")), none)

    assertEquals(WavelengthCoverage.decimalMicrometers.getOption(BigDecimal("1.5")),          WavelengthCoverage.intPicometers.getOption(1_500_000))
    assertEquals(WavelengthCoverage.decimalMicrometers.getOption(BigDecimal("1.0000004")),    WavelengthCoverage.intPicometers.getOption(1_000_000))
    assertEquals(WavelengthCoverage.decimalMicrometers.getOption(BigDecimal("1.0000005")),    WavelengthCoverage.intPicometers.getOption(1_000_001))
    assertEquals(WavelengthCoverage.decimalMicrometers.getOption(BigDecimal("2147.4836475")), none)

    assertEquals(WavelengthCoverage.fromIntMicrometers(Wavelength.MaxMicrometer + 1), none)
    assertEquals(WavelengthCoverage.fromIntNanometers(Wavelength.MaxNanometer + 1),   none)
    assertEquals(WavelengthCoverage.fromIntAngstroms(Wavelength.MaxAngstrom + 1),     none)
  }

  private def testFormat[U](format: Format[BigDecimal, WavelengthCoverage])(toPosBigDecimal: WavelengthCoverage => Quantity[PosBigDecimal, U]) =
    forAll { (w: WavelengthCoverage) =>
      assertEquals(
        format.getOption(toPosBigDecimal(w).value.value),
        w.some
      )
    }

  test("picometer decimal format") {
    testFormat(WavelengthCoverage.decimalPicometers) { w =>
      Quantity[Picometer](PosBigDecimal.unsafeFrom(BigDecimal(w.toPicometers.value.value)))
    }
  }

  test("angstrom decimal format") {
    testFormat(WavelengthCoverage.decimalAngstroms)(_.toAngstroms)
  }

  test("nanometers decimal format") {
    testFormat(WavelengthCoverage.decimalNanometers)(_.toNanometers)
  }

  test("micrometers decimal format") {
    testFormat(WavelengthCoverage.decimalMicrometers)(_.toMicrometers)
  }

  test("toAngstrom") {
    forAll { (a: PosInt) =>
      (a.value <= Wavelength.MaxAngstrom) ==> {
        assertEquals(WavelengthCoverage.fromIntAngstroms(a.value).map(_.toAngstroms.value.isWhole), true.some)
        assertEquals(WavelengthCoverage.fromIntAngstroms(a.value).map(_.toAngstroms.value), PosBigDecimal.unsafeFrom(a.value).some)
      }
    }
  }

  test("toNanometers") {
    forAll { (a: PosInt) =>
      (a.value <= Wavelength.MaxNanometer) ==> {
        assertEquals(WavelengthCoverage.fromIntNanometers(a.value).map(_.toAngstroms.value.isWhole), true.some)
        assertEquals(WavelengthCoverage.fromIntNanometers(a.value).map(_.toNanometers.value), PosBigDecimal.unsafeFrom(a.value).some)
      }
    }
  }

  test("toMicrons") {
    forAll { (a: PosInt) =>
      (a.value <= Wavelength.MaxMicrometer) ==> {
        assertEquals(WavelengthCoverage.fromIntMicrometers(a.value).map(_.toAngstroms.value.isWhole), true.some)
        assertEquals(WavelengthCoverage.fromIntMicrometers(a.value).map(_.toNanometers.value.isWhole), true.some)
        assertEquals(WavelengthCoverage.fromIntMicrometers(a.value).map(_.toMicrometers.value), PosBigDecimal.unsafeFrom(a.value).some)
      }
    }
  }

  test("centeredAt") {
    forAll{ (wc: WavelengthCoverage, w: Wavelength) =>
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
    forAll{ (wc: WavelengthCoverage, w: Wavelength) =>
      val range = wc.startingAt(w)
      assert(range.upper.pm.value.value - range.lower.pm.value.value <= wc.pm.value.value)
      (w.pm.value <= (Wavelength.Max.pm.value - wc.pm.value)) ==> {
        assertEquals(range.upper.pm.value.value - range.lower.pm.value.value, wc.pm.value.value)
      }
    }
  }

  test("endingAt") {
    forAll{ (wc: WavelengthCoverage, w: Wavelength) =>
      val range = wc.endingAt(w)
      assert(range.upper.pm.value.value - range.lower.pm.value.value <= wc.pm.value.value)
      (wc.pm < w.pm) ==> {
        assertEquals(range.upper.pm.value.value - range.lower.pm.value.value, wc.pm.value.value)
      }
    }
  }

}

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Eq
import cats.Order
import cats.Show
import cats.kernel.laws.discipline._
import lucuma.core.math.arb._
import lucuma.core.optics.laws.discipline.FormatTests
import lucuma.core.optics.laws.discipline.SplitEpiTests
import munit.DisciplineSuite
import org.scalacheck.Prop.forAll

final class BrightnessValueSuite extends DisciplineSuite {
  import ArbBrightnessValue._

  // Laws
  checkAll("BrightnessValue", OrderTests[BrightnessValue].order)
  checkAll("fromBigDecimal", SplitEpiTests(BrightnessValue.fromBigDecimal).splitEpi)
  checkAll("fromString", FormatTests(BrightnessValue.fromString).formatWith(stringsBrightnessValue))

  test("Equality must be natural") {
    forAll { (a: BrightnessValue, b: BrightnessValue) =>
      assertEquals(a.equals(b), Eq[BrightnessValue].eqv(a, b))
    }
  }

  test("Order must be consistent with .scaledValue") {
    forAll { (a: BrightnessValue, b: BrightnessValue) =>
      assertEquals(
        Order[Int].comparison(a.scaledValue, b.scaledValue),
        Order[BrightnessValue].comparison(a, b)
      )
    }
  }

  test("Show must be natural") {
    forAll { (a: BrightnessValue) =>
      assertEquals(a.toString, Show[BrightnessValue].show(a))
    }
  }

  test("Can extract the magnitude as a double") {
    forAll { (a: BrightnessValue) =>
      assertEquals(a.toDoubleValue, a.scaledValue / 1000.0)
    }
  }

  test("BrightnessValue rounding") {
    assertEquals(BrightnessValue.fromBigDecimal.get(BigDecimal("1.0004")).scaledValue, 1000)
    assertEquals(BrightnessValue.fromBigDecimal.get(BigDecimal("1.0005")).scaledValue, 1001)
  }

}

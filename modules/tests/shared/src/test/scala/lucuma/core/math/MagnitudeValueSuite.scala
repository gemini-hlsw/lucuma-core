// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.{ Eq, Order, Show }
import cats.kernel.laws.discipline._
import lucuma.core.math.arb._
import lucuma.core.optics.laws.discipline.FormatTests
import munit.DisciplineSuite
import org.scalacheck.Prop.forAll

final class MagnitudeValueSuite extends DisciplineSuite {
  import ArbMagnitudeValue._

  // Laws
  checkAll("MagnitudeValue", OrderTests[MagnitudeValue].order)
  checkAll("fromBigDecimal", FormatTests(MagnitudeValue.fromBigDecimal).format)

  test("Equality must be natural") {
    forAll { (a: MagnitudeValue, b: MagnitudeValue) =>
      assertEquals(a.equals(b), Eq[MagnitudeValue].eqv(a, b))
    }
  }

  test("Order must be consistent with .scaledValue") {
    forAll { (a: MagnitudeValue, b: MagnitudeValue) =>
      assertEquals(Order[Int].comparison(a.scaledValue, b.scaledValue),
                   Order[MagnitudeValue].comparison(a, b)
      )
    }
  }

  test("Show must be natural") {
    forAll { (a: MagnitudeValue) =>
      assertEquals(a.toString, Show[MagnitudeValue].show(a))
    }
  }

  test("Can extract the magnitude as a double") {
    forAll { (a: MagnitudeValue) =>
      assertEquals(a.toDoubleValue, a.scaledValue / 1000.0)
    }
  }

}

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.kernel.laws.discipline._
import cats.syntax.order._
import lucuma.core.math.arb._
import org.scalacheck.Prop._

final class CoverageSuite extends munit.DisciplineSuite {

  import ArbCoverage._
  import ArbWavelength._

  checkAll("Coverage", EqTests[Coverage].eqv)

  test("construction.invariant") {
    forAll { (a: Wavelength, b: Wavelength) =>
      assertEquals(Coverage(a, b).range.isDefined, a < b)
    }
  }

  test("intersection.identity") {
    forAll { (a: Coverage) =>
      assertEquals(a ⋂ a, a)
    }
  }

  test("intersection.annihilation.right") {
    forAll { (a: Coverage) =>
      assertEquals(a ⋂ Coverage.Empty, Coverage.Empty)
    }
  }

  test("intersection.annihilation.left") {
    forAll { (a: Coverage) =>
      assertEquals(Coverage.Empty ⋂ a, Coverage.Empty)
    }
  }

  test("intersection.reduction") {
    forAll { (a: Coverage, b: Coverage) =>
      assert((a ⋂ b).width.value.value <= a.width.value.value)
      assert((a ⋂ b).width.value.value <= b.width.value.value)
    }
  }

}

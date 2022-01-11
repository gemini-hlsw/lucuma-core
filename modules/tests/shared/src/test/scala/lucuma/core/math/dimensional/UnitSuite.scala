// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional

import cats.Eq
import cats.Show
import cats.kernel.laws.discipline._
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.dimensional.arb.ArbUnits
import lucuma.core.util.arb.ArbEnumerated._
import org.scalacheck.Prop._

class UnitSuite extends munit.DisciplineSuite {
  import ArbUnits._

  // Laws
  checkAll("Units", EqTests[Units].eqv)
  checkAll(
    "Units Of Brightness[Integrated]",
    EqTests[Units Of Brightness[Integrated]].eqv
  )

  test("Equality must be natural") {
    forAll { (a: Units, b: Units) =>
      assertEquals(a.equals(b), Eq[Units].eqv(a, b))
    }
  }

  test("Show must be natural") {
    forAll { (a: Units) =>
      assertEquals(a.toString, Show[Units].show(a))
    }
  }
}

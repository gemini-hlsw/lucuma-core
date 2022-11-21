// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional

import cats.Eq
import cats.kernel.laws.discipline._
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.dimensional.arb.ArbUnits
import lucuma.core.syntax.display._
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

  test("Display[Units]") {
    forAll { u: Units =>
      assertEquals(u.longName, u.name)
      assertEquals(u.shortName, u.abbv)
    }
  }

  test("Display[Units Of Brightness[Integrated]]") {
    forAll { u: Units Of Brightness[Integrated] =>
      assertEquals(u.longName, u.name)
      assertEquals(u.shortName, u.abbv)
    }
  }
}

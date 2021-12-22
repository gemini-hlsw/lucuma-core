// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Eq
import cats.Show
import cats.kernel.laws.discipline._
import lucuma.core.math.arb._
import lucuma.core.optics.laws.discipline._
import monocle.law.discipline._
import org.scalacheck.Prop._

final class OffsetQSuite extends munit.DisciplineSuite {
  import ArbAngle._
  import ArbOffset._

  // Laws
  checkAll("Offset.Component[Axis.Q].commutativeGroup",
           CommutativeGroupTests[Offset.Component[Axis.Q]].commutativeGroup
  )
  checkAll("Offset.Component[Axis.Q].order", OrderTests[Offset.Component[Axis.Q]].order)
  checkAll("Offset.Component.angle[Axis.Q]", IsoTests(Offset.Component.angle[Axis.Q]))
  checkAll("Offset.Component.signedArcseconds[Axis.Q]",
           SplitMonoTests(Offset.Component.signedDecimalArcseconds[Axis.Q]).splitMono
  )

  test("Equality must be natural") {
    forAll { (a: Offset.Component[Axis.Q], b: Offset.Component[Axis.Q]) =>
      assertEquals(a.equals(b),  Eq[Offset.Component[Axis.Q]].eqv(a, b))
    }
  }

  test("Equality be consistent with .toAngle") {
    forAll { (a: Offset.Component[Axis.Q], b: Offset.Component[Axis.Q]) =>
      assertEquals(Eq[Angle].eqv(a.toAngle, b.toAngle),  Eq[Offset.Component[Axis.Q]].eqv(a, b))
    }
  }

  test("Show must be natural") {
    forAll { (a: Offset.Component[Axis.Q]) =>
      assertEquals(a.toString,  Show[Offset.Component[Axis.Q]].show(a))
    }
  }

  test("Conversion to angle must be invertable") {
    forAll { (p: Offset.Component[Axis.Q]) =>
      assertEquals(Offset.Component[Axis.Q](p.toAngle),  p)
    }
  }

}

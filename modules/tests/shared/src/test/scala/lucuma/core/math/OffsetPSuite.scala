// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Eq
import cats.Show
import cats.kernel.laws.discipline.*
import lucuma.core.math.arb.*
import lucuma.core.optics.laws.discipline.*
import monocle.law.discipline.*
import org.scalacheck.Prop.*

final class OffsetPSuite extends munit.DisciplineSuite {
  import ArbAngle.given
  import ArbOffset.given

  // Laws
  checkAll("Offset.Component[Axis.P].commutativeGroup",
           CommutativeGroupTests[Offset.Component[Axis.P]].commutativeGroup
  )
  checkAll("Offset.Component[Axis.P].order", OrderTests[Offset.Component[Axis.P]].order)
  checkAll("Offset.Component.angle[Axis.P]", IsoTests(Offset.Component.angle[Axis.P]))
  checkAll("Offset.Component.signedArcseconds[Axis.P]",
           SplitMonoTests(Offset.Component.signedDecimalArcseconds[Axis.P]).splitMono
  )

  test("Equality must be natural") {
    forAll { (a: Offset.Component[Axis.P], b: Offset.Component[Axis.P]) =>
      assertEquals(a.equals(b),  Eq[Offset.Component[Axis.P]].eqv(a, b))
    }
  }

  test("Equality be consistent with .toAngle") {
    forAll { (a: Offset.Component[Axis.P], b: Offset.Component[Axis.P]) =>
      assertEquals(Eq[Angle].eqv(a.toAngle, b.toAngle),  Eq[Offset.Component[Axis.P]].eqv(a, b))
    }
  }

  test("Show must be natural") {
    forAll { (a: Offset.Component[Axis.P]) =>
      assertEquals(a.toString,  Show[Offset.Component[Axis.P]].show(a))
    }
  }

  test("Conversion to angle must be invertable") {
    forAll { (p: Offset.Component[Axis.P]) =>
      assertEquals(Offset.Component[Axis.P](p.toAngle),  p)
    }
  }

}

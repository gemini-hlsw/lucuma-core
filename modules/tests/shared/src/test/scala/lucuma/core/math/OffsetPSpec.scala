// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.tests.CatsSuite
import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import lucuma.core.optics.laws.discipline._
import lucuma.core.math.arb._
import monocle.law.discipline._

final class OffsetPSpec extends CatsSuite {
  import ArbAngle._
  import ArbOffset._

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
      a.equals(b) shouldEqual Eq[Offset.Component[Axis.P]].eqv(a, b)
    }
  }

  test("Equality be consistent with .toAngle") {
    forAll { (a: Offset.Component[Axis.P], b: Offset.Component[Axis.P]) =>
      Eq[Angle].eqv(a.toAngle, b.toAngle) shouldEqual Eq[Offset.Component[Axis.P]].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: Offset.Component[Axis.P]) =>
      a.toString shouldEqual Show[Offset.Component[Axis.P]].show(a)
    }
  }

  test("Conversion to angle must be invertable") {
    forAll { (p: Offset.Component[Axis.P]) =>
      Offset.Component[Axis.P](p.toAngle) shouldEqual p
    }
  }

}

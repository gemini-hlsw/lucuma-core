// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats.tests.CatsSuite
import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import gsp.math.laws.discipline._
import gsp.math.arb._
import monocle.law.discipline._

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class OffsetQSpec extends CatsSuite {
  import ArbAngle._
  import ArbOffset._

  // Laws
  checkAll("Offset.Component[Axis.Q].commutativeGroup", CommutativeGroupTests[Offset.Component[Axis.Q]].commutativeGroup)
  checkAll("Offset.Component[Axis.Q].order", OrderTests[Offset.Component[Axis.Q]].order)
  checkAll("Offset.Component.angle[Axis.Q]", IsoTests(Offset.Component.angle[Axis.Q]))
  checkAll("Offset.Component.signedArcseconds[Axis.Q]", SplitMonoTests(Offset.Component.signedArcseconds[Axis.Q]).splitMono)

  test("Equality must be natural") {
    forAll { (a: Offset.Component[Axis.Q], b: Offset.Component[Axis.Q]) =>
      a.equals(b) shouldEqual Eq[Offset.Component[Axis.Q]].eqv(a, b)
    }
  }

  test("Equality be consistent with .toAngle") {
    forAll { (a: Offset.Component[Axis.Q], b: Offset.Component[Axis.Q]) =>
      Eq[Angle].eqv(a.toAngle, b.toAngle) shouldEqual Eq[Offset.Component[Axis.Q]].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: Offset.Component[Axis.Q]) =>
      a.toString shouldEqual Show[Offset.Component[Axis.Q]].show(a)
    }
  }

  test("Conversion to angle must be invertable") {
    forAll { (p: Offset.Component[Axis.Q]) =>
      Offset.Component[Axis.Q](p.toAngle) shouldEqual p
    }
  }


}

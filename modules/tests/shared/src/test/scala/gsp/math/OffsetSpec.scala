// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats.tests.CatsSuite
import cats.{ Eq, Show }
import cats.kernel.laws.discipline._
import gsp.math.arb._
import monocle.law.discipline._

@SuppressWarnings(Array("org.wartremover.warts.ToString", "org.wartremover.warts.Equals"))
final class OffsetSpec extends CatsSuite {
  import ArbAngle._
  import ArbOffset._

  // Laws
  checkAll("Offset", CommutativeGroupTests[Offset].commutativeGroup)
  checkAll("Offset", OrderTests[Offset].order)
  checkAll("Axis.P", LensTests(Offset.p))
  checkAll("Axis.Q", LensTests(Offset.q))
  checkAll("Axis.PAngle", LensTests(Offset.pAngle))
  checkAll("Axis.QAngle", LensTests(Offset.qAngle))

  test("Equality must be natural") {
    forAll { (a: Offset, b: Offset) =>
      a.equals(b) shouldEqual Eq[Offset].eqv(a, b)
    }
  }

  test("it must operate pairwise") {
    forAll { (a: Offset, b: Offset) =>
      Eq[Offset.Component[Axis.P]].eqv(a.p, b.p) &&
      Eq[Offset.Component[Axis.Q]].eqv(a.q, b.q) shouldEqual Eq[Offset].eqv(a, b)
    }
  }

  test("Show must be natural") {
    forAll { (a: Offset) =>
      a.toString shouldEqual Show[Offset].show(a)
    }
  }

  test("Conversion to components must be invertable") {
    forAll { (o: Offset) =>
      val (p, q) = (o.p, o.q)
      Offset(p, q) shouldEqual o
    }
  }

}

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.implicits._
import cats.kernel.laws.discipline.{ BoundedEnumerableTests, OrderTests }
import eu.timepit.refined.api.Refined
import eu.timepit.refined.cats._
import eu.timepit.refined.char.Letter
import eu.timepit.refined.refineMV
import eu.timepit.refined.scalacheck.all._
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.util.arb.ArbGid._
import monocle.law.discipline.{ IsoTests, PrismTests }
import munit._
import org.scalacheck.Prop._

final class GidSuite extends DisciplineSuite {

  case class Id(value: PosLong)
  object Id {
    implicit val gid: Gid[Id] = Gid.instance(refineMV('i'), _.value, apply)
  }

  checkAll("Gid[Id]", BoundedEnumerableTests[Id].boundedEnumerable)
  checkAll("Gid[Id]", OrderTests[Id].order)
  checkAll("Gid[Id].isoPosLong", IsoTests(Gid[Id].isoPosLong))
  checkAll("Gid[Id].fromString", PrismTests(Gid[Id].fromString))

  test("fromString: Tag") {
    forAll { (c: Char Refined Letter, n: PosLong) =>
      val s = s"${c.value}-${n.value.toHexString}"
      assert(
        Gid[Id].fromString.getOption(s).isDefined == (c === Gid[Id].tag),
        s"$s should parse iff $c === ${Gid[Id].tag}"
      )
    }
  }

}

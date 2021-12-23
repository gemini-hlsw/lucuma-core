// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional.arb

import coulomb.Unitless
import lucuma.core.math.dimensional._
import org.scalacheck.Arbitrary._
import org.scalacheck._
import shapeless.tag.@@

trait ArbQty {
  implicit val arbUnitType: Arbitrary[UnitType] =
    Arbitrary {
      for {
        _name <- arbitrary[String]
        _abbv <- arbitrary[String]
      } yield new UnitType {
        type Type = Unitless
        val name = _name
        val abbv = _abbv
      }
    }

  implicit val cogenUnitType: Cogen[UnitType] =
    Cogen[(String, String)].contramap(x => (x.name, x.abbv))

  implicit def arbQty[N: Arbitrary]: Arbitrary[Qty[N]] =
    Arbitrary {
      for {
        n <- arbitrary[N]
        u <- arbitrary[UnitType]
      } yield u.withValue(n)
    }

  implicit def cogenQty[N: Cogen]: Cogen[Qty[N]] =
    Cogen[(N, UnitType)].contramap(q => (q.value, q.unit))

  implicit def arbTaggedUnitQty[N: Arbitrary, Tag](implicit
    arbUnit: Arbitrary[UnitType @@ Tag]
  ): Arbitrary[Qty[N] @@ Tag] =
    Arbitrary {
      for {
        n <- arbitrary[N]
        u <- arbitrary[UnitType @@ Tag]
      } yield u.withValueT(n)
    }

  implicit def cogenTaggedUnitQty[N: Cogen, Tag]: Cogen[Qty[N] @@ Tag] =
    Cogen[(N, UnitType)].contramap(q => (q.value, q.unit))
}

object ArbQty extends ArbQty

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional.arb

// import coulomb.define.UnitDefinition
import lucuma.core.math.dimensional._
import org.scalacheck.Arbitrary._
import org.scalacheck._
import shapeless.tag.@@

trait ArbQty {
  implicit val cogenUnitType: Cogen[UnitType] =
    Cogen[(String, String)].contramap(x => (x.name, x.abbv))

  implicit def arbQty[N: Arbitrary, U](implicit
    unit: UnitOfMeasure[U]
  ): Arbitrary[Qty[N]] =
    Arbitrary {
      for {
        n <- arbitrary[N]
      } yield unit.withValue(n)
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

  // implicit def cogenTaggedUnitQty[N: Cogen, Tag](implicit
  //   cogenUnit: Cogen[UnitType @@ Tag]
  // ): Cogen[UnitType @@ Tag] =
  //   Cogen[(N, UnitType @@ Tag)].contramap(q => (q.value, q.unit))
}

object ArbQty extends ArbQty

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional.arb

// import coulomb.define.UnitDefinition
import coulomb.define.UnitDefinition
import lucuma.core.math.dimensional._
import org.scalacheck.Arbitrary._
import org.scalacheck._

trait ArbQty {
  implicit val cogenUnitDefinition: Cogen[UnitDefinition] =
    Cogen[(String, String)].contramap(x => (x.name, x.abbv))

  implicit val cogenUnitType: Cogen[UnitType] = Cogen[UnitDefinition].contramap(_.definition)

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

  implicit def arbGroupedUnitQty[N: Arbitrary, UG](implicit
    arbUnit: Arbitrary[GroupedUnitType[UG]]
  ): Arbitrary[GroupedUnitQty[N, UG]] =
    Arbitrary {
      for {
        n <- arbitrary[N]
        u <- arbitrary[GroupedUnitType[UG]]
      } yield u.withValue(n)
    }

  implicit def cogenGroupedUnitQty[N: Cogen, UG](implicit
    cogenUnit: Cogen[GroupedUnitType[UG]]
  ): Cogen[GroupedUnitQty[N, UG]] =
    Cogen[(N, GroupedUnitType[UG])].contramap(q => (q.value, q.unit))
}

object ArbQty extends ArbQty

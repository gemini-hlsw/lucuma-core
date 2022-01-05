// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional.arb

import coulomb.Unitless
import lucuma.core.math.dimensional._
import org.scalacheck.Arbitrary._
import org.scalacheck._

trait ArbMeasure {
  implicit val arbUnitType: Arbitrary[Units] =
    Arbitrary {
      for {
        name <- arbitrary[String]
        abbv <- arbitrary[String]
        tag  <- arbitrary[String]
      } yield UnitOfMeasure[Unitless](name, abbv, tag)
    }

  implicit val cogenUnitType: Cogen[Units] =
    Cogen[(String, String)].contramap(x => (x.name, x.abbv))

  implicit def arbMeasure[N: Arbitrary]: Arbitrary[Measure[N]] =
    Arbitrary {
      for {
        n <- arbitrary[N]
        u <- arbitrary[Units]
        e <- arbitrary[Option[N]]
      } yield u.withValue(n, e)
    }

  implicit def cogenMeasure[N: Cogen]: Cogen[Measure[N]] =
    Cogen[(N, Units, Option[N])].contramap(m => (m.value, m.units, m.error))

  implicit def arbTaggedUnitMeasure[N: Arbitrary, Tag](implicit
    arbUnit: Arbitrary[Units Of Tag]
  ): Arbitrary[Measure[N] Of Tag] =
    Arbitrary {
      for {
        n <- arbitrary[N]
        u <- arbitrary[Units Of Tag]
        e <- arbitrary[Option[N]]
      } yield u.withValueTagged(n, e)
    }

  implicit def cogenTaggedUnitMeasure[N: Cogen, Tag]: Cogen[Measure[N] Of Tag] =
    Cogen[(N, Units, Option[N])].contramap(m => (m.value, m.units, m.error))
}

object ArbMeasure extends ArbMeasure

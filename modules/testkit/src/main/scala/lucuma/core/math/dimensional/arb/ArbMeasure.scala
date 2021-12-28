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
        _name <- arbitrary[String]
        _abbv <- arbitrary[String]
      } yield new Units {
        type Type = Unitless
        val name = _name
        val abbv = _abbv
      }
    }

  implicit val cogenUnitType: Cogen[Units] =
    Cogen[(String, String)].contramap(x => (x.name, x.abbv))

  implicit def arbMeasure[N: Arbitrary]: Arbitrary[Measure[N]] =
    Arbitrary {
      for {
        n <- arbitrary[N]
        u <- arbitrary[Units]
      } yield u.withValue(n)
    }

  implicit def cogenMeasure[N: Cogen]: Cogen[Measure[N]] =
    Cogen[(N, Units)].contramap(q => (q.value, q.units))

  implicit def arbTaggedUnitMeasure[N: Arbitrary, Tag](implicit
    arbUnit: Arbitrary[Units Of Tag]
  ): Arbitrary[Measure[N] Of Tag] =
    Arbitrary {
      for {
        n <- arbitrary[N]
        u <- arbitrary[Units Of Tag]
      } yield u.withValueTagged(n)
    }

  implicit def cogenTaggedUnitMeasure[N: Cogen, Tag]: Cogen[Measure[N] Of Tag] =
    Cogen[(N, Units)].contramap(q => (q.value, q.units))
}

object ArbMeasure extends ArbMeasure

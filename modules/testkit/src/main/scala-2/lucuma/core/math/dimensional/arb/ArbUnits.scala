// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.dimensional.arb

import coulomb.Unitless
import lucuma.core.math.dimensional._
import org.scalacheck.Arbitrary._
import org.scalacheck._

trait ArbUnits {
  implicit val arbUnits: Arbitrary[Units] =
    Arbitrary {
      for {
        name       <- arbitrary[String]
        abbv       <- arbitrary[String]
        serialized <- arbitrary[String]
      } yield UnitOfMeasure[Unitless](name, abbv, serialized)
    }

  implicit val cogenUnits: Cogen[Units] =
    Cogen[(String, String, String)].contramap(x => (x.name, x.abbv, x.serialized))
}

object ArbUnits extends ArbUnits

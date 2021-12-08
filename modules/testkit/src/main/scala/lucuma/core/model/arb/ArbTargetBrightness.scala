// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import coulomb.define.UnitDefinition
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessValue
import lucuma.core.math.arb.ArbBrightnessValue._
import lucuma.core.math.dimensional._
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import org.scalacheck._

trait ArbTargetBrightness {

  import ArbEnumerated._

  implicit def arbTargetBrightness[B](implicit
    arbUnit: Arbitrary[GroupedUnitType[B]]
  ): Arbitrary[TargetBrightness[B]] =
    Arbitrary {
      for {
        s <- arbitrary[GroupedUnitType[B]]
        v <- arbitrary[BrightnessValue]
        b <- arbitrary[Band]
        e <- arbitrary[Option[BrightnessValue]]
      } yield TargetBrightness(s.withValue(v), b, e)
    }

  // TODO Move these two to a separate file, testing the whole dimensional package.
  implicit val cogUnitDefinition: Cogen[UnitDefinition] =
    Cogen[(String, String)].contramap(u => (u.name, u.abbv))

  implicit def cogQuantityTrait[N: Cogen]: Cogen[Qty[N]] =
    Cogen[(N, UnitDefinition)].contramap(q => (q.value, q.unit.definition))
  // END Move to another file

  implicit def cogTargetBrightness[B]: Cogen[TargetBrightness[B]] =
    Cogen[
      (Qty[BrightnessValue], Band, Option[BrightnessValue])
    ].contramap(u => (u.quantity, u.band, u.error))

  // Move to ArbBrightnessProfile
  // implicit val arbBrightnessesMap: Arbitrary[SortedMap[Band, TargetBrightness]] =
  //   Arbitrary(
  //     arbitrary[Vector[TargetBrightness]].map(_.fproductLeft(_.band)).map(x => SortedMap(x: _*))
  //   )

  // implicit val cogBrightnessesMap: Cogen[SortedMap[Band, TargetBrightness]] =
  //   Cogen[Vector[(Band, TargetBrightness)]].contramap(_.toVector)
}

object ArbTargetBrightness extends ArbTargetBrightness

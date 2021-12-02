// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import cats.implicits._
import coulomb.define.UnitDefinition
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnit
import lucuma.core.math.BrightnessValue
import lucuma.core.math.arb.ArbBrightnessValue._
import lucuma.core.math.dimensional._
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import org.scalacheck._

import scala.collection.immutable.SortedMap

trait ArbTargetBrightness {

  import ArbEnumerated._

  implicit val arbTargetBrightness: Arbitrary[TargetBrightness] =
    Arbitrary {
      for {
        s <- Gen.oneOf(
               arbitrary[GroupedUnitType[BrightnessUnit.Integrated]],
               arbitrary[GroupedUnitType[BrightnessUnit.Surface]]
             )
        v <- arbitrary[BrightnessValue]
        b <- arbitrary[Band]
        e <- arbitrary[Option[BrightnessValue]]
      } yield TargetBrightness(s.withValue(v), b, e)
    }

  implicit val cogUnitDefinition: Cogen[UnitDefinition] =
    Cogen[(String, String)].contramap(u => (u.name, u.abbv))

  implicit def cogQuantityTrait[N: Cogen]: Cogen[Qty[N]] =
    Cogen[(N, UnitDefinition)].contramap(q => (q.value, q.unit.definition))

  implicit val cogBrightness: Cogen[TargetBrightness] =
    Cogen[
      (Qty[BrightnessValue], Band, Option[BrightnessValue])
    ].contramap(u => (u.quantity, u.band, u.error))

  implicit val arbBrightnessesMap: Arbitrary[SortedMap[Band, TargetBrightness]] =
    Arbitrary(
      arbitrary[Vector[TargetBrightness]].map(_.fproductLeft(_.band)).map(x => SortedMap(x: _*))
    )

  implicit val cogBrightnessesMap: Cogen[SortedMap[Band, TargetBrightness]] =
    Cogen[Vector[(Band, TargetBrightness)]].contramap(_.toVector)
}

object ArbTargetBrightness extends ArbTargetBrightness

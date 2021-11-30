// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import cats.implicits._
import lucuma.core.enum.Band
import lucuma.core.enum.BrightnessUnits
import lucuma.core.math.BrightnessValue
import lucuma.core.math.arb.ArbBrightnessValue._
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import org.scalacheck._

import scala.collection.immutable.SortedMap

trait ArbBrightness {

  import ArbEnumerated._

  implicit val arbBrightness: Arbitrary[Brightness] =
    Arbitrary {
      for {
        v <- arbitrary[BrightnessValue]
        b <- arbitrary[Band]
        e <- arbitrary[Option[BrightnessValue]]
        s <- arbitrary[BrightnessUnits]
      } yield Brightness(v, b, e, s)
    }

  implicit val cogBrightness: Cogen[Brightness] =
    Cogen[(BrightnessValue, Band, Option[BrightnessValue], BrightnessUnits)].contramap { u =>
      (u.value, u.band, u.error, u.units)
    }

  implicit val arbBrightnessesMap: Arbitrary[SortedMap[Band, Brightness]] =
    Arbitrary(arbitrary[Vector[Brightness]].map(_.fproductLeft(_.band)).map(x => SortedMap(x: _*)))

  implicit val cogBrightnessesMap: Cogen[SortedMap[Band, Brightness]] =
    Cogen[Vector[(Band, Brightness)]].contramap(_.toVector)
}

object ArbBrightness extends ArbBrightness

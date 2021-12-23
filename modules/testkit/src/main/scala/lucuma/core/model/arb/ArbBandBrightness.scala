// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnits
import lucuma.core.math.BrightnessValue
import lucuma.core.math.arb.ArbBrightnessValue._
import lucuma.core.math.dimensional._
import lucuma.core.math.dimensional.arb.ArbQty
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import org.scalacheck._
import shapeless.tag.@@

trait ArbBandBrightness {
  import ArbEnumerated._
  import BrightnessUnits._
  import ArbQty._

  implicit def arbBandBrightness[T](implicit
    arbUnit: Arbitrary[UnitType @@ Brightness[T]]
  ): Arbitrary[BandBrightness[T]] =
    Arbitrary {
      for {
        q <- arbitrary[Qty[BrightnessValue] @@ Brightness[T]]
        b <- arbitrary[Band]
        e <- arbitrary[Option[BrightnessValue]]
      } yield BandBrightness(q, b, e)
    }

  implicit def cogBandBrightness[T]: Cogen[BandBrightness[T]] =
    Cogen[
      (Qty[BrightnessValue], Band, Option[BrightnessValue])
    ].contramap(u => (u.quantity, u.band, u.error))
}

object ArbBandBrightness extends ArbBandBrightness

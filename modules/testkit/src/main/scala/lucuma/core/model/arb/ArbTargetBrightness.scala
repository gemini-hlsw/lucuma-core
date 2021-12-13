// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnit
import lucuma.core.math.BrightnessValue
import lucuma.core.math.arb.ArbBrightnessValue._
import lucuma.core.math.dimensional._
import lucuma.core.math.dimensional.arb.ArbQty
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import org.scalacheck._

trait ArbTargetBrightness {
  import ArbEnumerated._
  import BrightnessUnit._
  import ArbQty._

  implicit def arbTargetBrightness[T](implicit
    arbUnit: Arbitrary[GroupedUnitType[Brightness[T]]]
  ): Arbitrary[TargetBrightness[T]] =
    Arbitrary {
      for {
        q <- arbitrary[GroupedUnitQuantity[BrightnessValue, Brightness[T]]]
        b <- arbitrary[Band]
        e <- arbitrary[Option[BrightnessValue]]
      } yield TargetBrightness(q, b, e)
    }

  implicit def cogTargetBrightness[B]: Cogen[TargetBrightness[B]] =
    Cogen[
      (Qty[BrightnessValue], Band, Option[BrightnessValue])
    ].contramap(u => (u.quantity, u.band, u.error))
}

object ArbTargetBrightness extends ArbTargetBrightness

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.Order
import coulomb.Quantity
import eu.timepit.refined.predicates.numeric.NonNegative
import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.math.units.MilliVegaMagnitude
import lucuma.core.math.units.VegaMagnitude
import lucuma.core.math.units.conversions.given
import lucuma.core.optics.Format
import lucuma.core.util.NewRefinedQuantity
import monocle.Prism
import spire.math.Rational

/**
 * Extinction in mags, a non-negative number with three decimal points of precision,
 * in [0.000, 32.767].
 */
type Extinction = Extinction.Type
object Extinction extends NewRefinedQuantity[Short, NonNegative, MilliVegaMagnitude]:
  val FromMilliVegaMagnitude: Prism[Short, Extinction] =
    Prism((s: Short) => from(s).toOption)(_.value.value.value)

  val FromVegaMagnitude: Format[BigDecimal, Extinction] =
    Format(
      d => FromMilliVegaMagnitude.getOption(d.bigDecimal.movePointRight(3).shortValue),
      e => BigDecimal(FromMilliVegaMagnitude.reverseGet(e)).bigDecimal.movePointLeft(3)
    )

  given Order[Extinction] =
    Order.by(FromMilliVegaMagnitude.reverseGet)

  given Encoder[Extinction] =
    Encoder[BigDecimal].contramap(FromVegaMagnitude.reverseGet)

  given Decoder[Extinction] =
    Decoder[BigDecimal].emap(d => FromVegaMagnitude.getOption(d).toRight(s"Invalid extinction: $d"))

  extension (e: Extinction)
    def toMilliVegaMagnitude: Quantity[NonNegShort, MilliVegaMagnitude] = e.value
    def toVegaMagnitude: Quantity[Rational, VegaMagnitude] = e.toMilliVegaMagnitude.toValue[Short].toValue[Rational].toUnit[VegaMagnitude]
    def transmission: Double = math.pow(10.0, toVegaMagnitude.value.toDouble / -2.5)



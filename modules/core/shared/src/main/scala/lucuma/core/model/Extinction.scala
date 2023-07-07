// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.Order
import eu.timepit.refined.types.numeric.NonNegShort
import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.optics.Format
import monocle.Prism

/** 
 * Extinction in mags, a non-negative number with two decimal points of precision, 
 * in [0.00, 327.67].
 */
opaque type Extinction = NonNegShort

object Extinction:

  def apply(millimags: NonNegShort): Extinction =
    millimags

  val FromMillimags: Prism[Short, Extinction] =
    Prism((s: Short) => NonNegShort.from(s).toOption)(_.value)

  val FromMags: Format[BigDecimal, Extinction] =
    Format(
      d => FromMillimags.getOption(d.bigDecimal.movePointRight(2).shortValue),
      e => BigDecimal(FromMillimags.reverseGet(e)).bigDecimal.movePointLeft(2) 
    )

  given Order[Extinction] =
    Order.by(_.value)

  given Encoder[Extinction] =
    Encoder[BigDecimal].contramap(FromMags.reverseGet)

  given Decoder[Extinction] =
    Decoder[BigDecimal].emap(d => FromMags.getOption(d).toRight(s"Invalid extinction: $d"))

  extension (e: Extinction)
    def underlying: NonNegShort = e
    def transmission: Double = math.pow(10.0, e.value * 1000.0 / -2.5)
  
  
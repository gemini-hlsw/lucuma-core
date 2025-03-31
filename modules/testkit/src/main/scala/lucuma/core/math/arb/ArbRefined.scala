// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import eu.timepit.refined.api.Max
import eu.timepit.refined.api.Min
import eu.timepit.refined.api.Refined
import eu.timepit.refined.internal.Adjacent
import org.scalacheck.Cogen


trait ArbRefined {
  given Adjacent[BigDecimal] = Adjacent.instance[BigDecimal](
    _.compare(_),
    x => BigDecimal(Adjacent[Double].nextUp(x.toDouble)),
    x => BigDecimal(Adjacent[Double].nextDown(x.toDouble))
  )

  given Max[BigDecimal] = new Max[BigDecimal]:
    val max: BigDecimal = BigDecimal(Double.MaxValue)

  given Min[BigDecimal] = new Min[BigDecimal]:
    val min: BigDecimal = BigDecimal(Double.MinValue)

  given cogenRefined[A: Cogen, P]: Cogen[A Refined P] =
    Cogen[A].contramap(_.value)
}

object ArbRefined extends ArbRefined

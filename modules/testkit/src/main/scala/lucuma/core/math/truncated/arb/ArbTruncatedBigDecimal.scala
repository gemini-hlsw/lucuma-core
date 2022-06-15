// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.truncated.arb

import lucuma.core.math.truncated.TruncatedBigDecimal
import org.scalacheck.Arbitrary._
import org.scalacheck._

trait ArbTruncatedBigDecimal {

  implicit val arbTruncatedDecimal = Arbitrary[TruncatedBigDecimal[2]] {
    arbitrary[BigDecimal].map(TruncatedBigDecimal[2](_))
  }

  implicit def cogTruncatedBigDecimal: Cogen[TruncatedBigDecimal[2]] =
    Cogen[BigDecimal].contramap(_.value)
}

object ArbTruncatedBigDecimal extends ArbTruncatedBigDecimal

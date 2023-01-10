// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.model.NonNegDuration
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

import java.time.Duration

trait ArbNonNegDuration {

  given Arbitrary[NonNegDuration] =
    Arbitrary {
      for {
        s <- Gen.chooseNum(0L, Long.MaxValue / ArbNonNegDuration.NANOS_PER_SECOND - 1)
        n <- Gen.chooseNum(0L, 999999999L)
      } yield NonNegDuration.unsafeFrom(Duration.ofSeconds(s, n))
    }

  given Cogen[NonNegDuration] =
    Cogen[(Long, Int)].contramap(a => (a.value.getSeconds, a.value.getNano))

}

object ArbNonNegDuration extends ArbNonNegDuration {

  private val NANOS_PER_SECOND: Long =
    1000 * 1000 * 1000

}

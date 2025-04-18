// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbTimeSpan
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbConfigChangeEstimate {

  import ArbTimeSpan.given

  given Arbitrary[ConfigChangeEstimate] =
    Arbitrary {
      for {
        n <- arbitrary[String]
        d <- arbitrary[String]
        e <- arbitrary[TimeSpan]
      } yield ConfigChangeEstimate(n, d, e)
    }

  given Cogen[ConfigChangeEstimate] =
    Cogen[(String, String, TimeSpan)].contramap { a =>
      (a.name, a.description, a.estimate)
    }
}

object ArbConfigChangeEstimate extends ArbConfigChangeEstimate

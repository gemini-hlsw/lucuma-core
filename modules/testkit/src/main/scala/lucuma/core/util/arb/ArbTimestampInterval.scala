// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util
package arb

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

trait ArbTimestampInterval {

  import ArbTimestamp.*

  given Arbitrary[TimestampInterval] =
    Arbitrary {
      for {
        t0 <- arbitrary[Timestamp]
        t1 <- arbitrary[Timestamp]
      } yield TimestampInterval.between(t0, t1)
    }

  given Cogen[TimestampInterval] =
    Cogen[(Timestamp, Timestamp)].contramap { a => (a.start, a.end) }

}

object ArbTimestampInterval extends ArbTimestampInterval

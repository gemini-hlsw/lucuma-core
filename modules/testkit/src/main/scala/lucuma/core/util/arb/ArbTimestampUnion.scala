// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util
package arb

import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbTimestampUnion {

  import ArbTimestampInterval.given

  given Arbitrary[TimestampUnion] =
    Arbitrary {
      for {
        sz <- Gen.chooseNum(0, 10)
        ts <- Gen.listOfN(sz, arbitrary[TimestampInterval])
      } yield TimestampUnion.fromIntervals(ts)
    }

  given Cogen[TimestampUnion] =
    Cogen[List[TimestampInterval]].contramap { a =>
      a.intervals.toList
    }
}

object ArbTimestampUnion extends ArbTimestampUnion

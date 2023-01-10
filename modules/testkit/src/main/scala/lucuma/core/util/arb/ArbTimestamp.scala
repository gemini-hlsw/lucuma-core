// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util
package arb

import lucuma.core.arb.ArbTime
import lucuma.core.util.Timestamp
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

import java.time._

// Arbitrary but reasonable Timestamp
trait ArbTimestamp {
  import ArbTime.cogInstant

  implicit val arbTimestamp: Arbitrary[Timestamp] =
    Arbitrary {
      for {
        m <- Gen.choose(0L, Duration.between(Instant.EPOCH, Timestamp.Max.toInstant).toMillis)
        u <- Gen.choose(0L, 999L)
      } yield Timestamp.Epoch.plusMillisOption(m).flatMap(_.plusMicrosOption(u)).getOrElse(Timestamp.Epoch)
    }

  implicit val cogTimestamp: Cogen[Timestamp] =
    Cogen[Instant].contramap(_.toInstant)

  val genTimestampString: Gen[String] =
    Gen.oneOf(
      arbitrary[Timestamp].map(_.format),
      arbitrary[Timestamp].map(_.format).map(s => s"${s}000"),
      arbitrary[Timestamp].map(_.isoFormat),
      arbitrary[(Timestamp, Int, Char)].map { case (t, i, c) =>
        val cs = t.format.toCharArray
        val in = (i % cs.size).abs
        cs(in) = c
        String.valueOf(cs)
      }
    )

}

object ArbTimestamp extends ArbTimestamp

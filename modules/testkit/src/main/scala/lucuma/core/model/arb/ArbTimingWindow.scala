// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import cats.syntax.either.*
import eu.timepit.refined.scalacheck.numeric.given
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.TimingWindowInclusion
import lucuma.core.math.arb.ArbRefined
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbTimeSpan
import lucuma.core.util.arb.ArbTimestamp
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.given

trait ArbTimingWindow {
  import ArbEnumerated.given
  import ArbTimestamp.given
  import ArbTimeSpan.given
  import ArbRefined.given

  given Arbitrary[TimingWindowRepeat] =
    Arbitrary(
      for
        period <- arbitrary[TimeSpan]
        times  <- arbitrary[Option[PosInt]]
      yield TimingWindowRepeat(period, times)
    )

  given Arbitrary[TimingWindowEnd.At] =
    Arbitrary(arbitrary[Timestamp].map(TimingWindowEnd.At(_)))

  given Arbitrary[TimingWindowEnd.After] =
    Arbitrary(
      for
        duration <- arbitrary[TimeSpan]
        repeat   <- arbitrary[Option[TimingWindowRepeat]]
      yield TimingWindowEnd.After(duration, repeat)
    )

  given Arbitrary[TimingWindowEnd] =
    Arbitrary(
      Gen.oneOf(arbitrary[TimingWindowEnd.At], arbitrary[TimingWindowEnd.After])
    )

  given Arbitrary[TimingWindow] =
    Arbitrary(
      for
        inclusion <- arbitrary[TimingWindowInclusion]
        start     <- arbitrary[Timestamp]
        end       <- arbitrary[Option[TimingWindowEnd]]
      yield TimingWindow(inclusion, start, end)
    )

  given Cogen[TimingWindowRepeat] =
    Cogen[(TimeSpan, Option[PosInt])].contramap(twr => (twr.period, twr.times))

  given Cogen[TimingWindowEnd.At] = Cogen[Timestamp].contramap(_.instant)

  given Cogen[TimingWindowEnd.After] =
    Cogen[(TimeSpan, Option[TimingWindowRepeat])].contramap(twea => (twea.duration, twea.repeat))

  given Cogen[TimingWindowEnd] =
    Cogen[Either[TimingWindowEnd.At, TimingWindowEnd.After]].contramap {
      case at @ TimingWindowEnd.At(_)          => at.asLeft
      case after @ TimingWindowEnd.After(_, _) => after.asRight
    }

  given Cogen[TimingWindow] =
    Cogen[(TimingWindowInclusion, Timestamp, Option[TimingWindowEnd])]
      .contramap(tw => (tw.inclusion, tw.start, tw.end))
}

object ArbTimingWindow extends ArbTimingWindow

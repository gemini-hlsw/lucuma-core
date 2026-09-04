// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.OrderTests
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.TimingWindowInclusion
import lucuma.core.math.BoundedInterval
import lucuma.core.model.arb.ArbTimingWindow
import lucuma.core.syntax.time.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import munit.*
import org.typelevel.cats.time.given

import java.time.Duration
import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.*

final class TimingWindowSuite extends DisciplineSuite {
  import ArbTimingWindow.given

  // Every test here should run in milliseconds.  The bound exists for the
  // "tractable" test below: munit runs each test body under this timeout (on
  // the JVM, synchronous tests included), so a quadratic regression fails at
  // ten seconds instead of grinding for minutes.
  override def munitTimeout: FiniteDuration = 10.seconds

  // Laws
  checkAll("Order[TimingWindow]", OrderTests[TimingWindow].eqv)

  private val lower: Instant = Instant.parse("2026-02-01T00:00:00Z")

  private def within(days: Long): BoundedInterval[Instant] =
    BoundedInterval.unsafeOpenUpper(lower, lower.plus(days, ChronoUnit.DAYS))

  private def repeating(start: Instant, duration: Duration, period: Duration, times: Option[Int]): TimingWindow =
    TimingWindow(
      TimingWindowInclusion.Include,
      Timestamp.unsafeFromInstant(start),
      Some(
        TimingWindowEnd.After(
          TimeSpan.unsafeFromDuration(duration),
          Some(TimingWindowRepeat(TimeSpan.unsafeFromDuration(period), times.map(PosInt.unsafeFrom)))
        )
      )
    )

  test("toIntervalSeq: repeat forever unfolds one interval per period within the bounds") {
    val seq = repeating(lower, Duration.ofHours(1), Duration.ofDays(1), None).toIntervalSeq(within(10))
    assertEquals(seq.intervals.size, 10)
    assertEquals(seq.duration, Duration.ofHours(10))
  }

  test("toIntervalSeq: repeat n times stops after the repeats even when the bounds continue") {
    val seq = repeating(lower, Duration.ofHours(1), Duration.ofDays(1), Some(3)).toIntervalSeq(within(10))
    // The initial window plus three repeats.
    assertEquals(seq.intervals.size, 4)
    assertEquals(seq.duration, Duration.ofHours(4))
  }

  test("toIntervalSeq: a repeating window starting after the bounds is empty") {
    val seq = repeating(lower.plus(20, ChronoUnit.DAYS), Duration.ofHours(1), Duration.ofDays(1), None)
      .toIntervalSeq(within(10))
    assert(seq.isEmpty)
  }

  test("toIntervalSeq: a short period over a long interval is tractable") {
    // 30 seconds every minute for 180 days: 259,200 repeats.  A left fold of
    // spire unions over that many singletons is quadratic and takes minutes;
    // this must complete within `munitTimeout` (and does, in well under a second).
    val seq = repeating(lower, Duration.ofSeconds(30), Duration.ofMinutes(1), None).toIntervalSeq(within(180))
    assertEquals(seq.intervals.size, 259200)
    assertEquals(seq.duration, Duration.ofDays(90))
  }

}

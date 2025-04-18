// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Order
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.TimingWindowInclusion
import lucuma.core.math.BoundedInterval
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism
import org.typelevel.cats.time.given
import spire.math.Interval
import spire.math.extras.interval.IntervalSeq

import java.time.Duration
import java.time.Instant

/** A repetition period for timing windows, with optional repetition times * */
case class TimingWindowRepeat(period: TimeSpan, times: Option[PosInt]) derives Eq

object TimingWindowRepeat:
  def period(value: TimeSpan) = TimingWindowRepeat(value, None)

  val period: Lens[TimingWindowRepeat, TimeSpan]      = Focus[TimingWindowRepeat](_.period)
  val times: Lens[TimingWindowRepeat, Option[PosInt]] = Focus[TimingWindowRepeat](_.times)

/**
 * End of a timing window at a specific point in time, or after a specified duration with optional
 * repetition. *
 */
enum TimingWindowEnd derives Eq:
  case At(instant: Timestamp)                                        extends TimingWindowEnd
  case After(duration: TimeSpan, repeat: Option[TimingWindowRepeat]) extends TimingWindowEnd

object TimingWindowEnd:
  object At:
    val instant: Lens[At, Timestamp] = Focus[At](_.instant)

  object After:
    val duration: Lens[After, TimeSpan]                 = Focus[After](_.duration)
    val repeat: Lens[After, Option[TimingWindowRepeat]] = Focus[After](_.repeat)

  val at: Prism[TimingWindowEnd, At]       = GenPrism[TimingWindowEnd, At]
  val after: Prism[TimingWindowEnd, After] = GenPrism[TimingWindowEnd, After]

/** Timing window definition. * */
final case class TimingWindow(
  inclusion: TimingWindowInclusion,
  start:     Timestamp,
  end:       Option[TimingWindowEnd]
):
  def duration: Option[TimeSpan] =
    end.flatMap {
      case TimingWindowEnd.At(ts)      => TimeSpan.between(start, ts)
      case TimingWindowEnd.After(d, _) => d.some
    }

  def isValid: Boolean =
    end.flatMap(TimingWindowEnd.at.getOption).forall(_.instant > start)

  /**
   * The windows defined by this TimingWindow defintion, capped to the provided `within` interval.
   *
   * The result is provided as a (spire's) `IntervalSeq[Instant]`.
   */
  def toIntervalSeq(within: BoundedInterval[Instant]): IntervalSeq[Instant] = {
    // Builds a bunch of single-interval `IntervalSeq`s, for each of the `starts` provided, each lasting `duration`.
    // Returns the union of all of them.
    def intervalsForStarts(starts: List[Instant], duration: Duration): IntervalSeq[Instant] =
      starts
        .map(start => IntervalSeq(Interval(start, start.plus(duration))))
        .foldLeft(IntervalSeq.empty[Instant])(_ | _)

    val windowStart = start.toInstant

    // find the start of a repeat window nearest to the start of `within`
    def windowStartForPeriod(period: TimeSpan) =
      windowStart
        .plusMillis(
          period.toDuration
            .multipliedBy(
              Duration.between(windowStart, within.lower).toMillis() / period.toDuration
                .toMillis()
            )
            .toMillis()
        )
        .max(windowStart) // window start could be set after `within`

    val intervals =
      end match {
        case None                                                                                 =>
          IntervalSeq.atOrAbove(windowStart)
        case Some(TimingWindowEnd.At(instant))                                                    =>
          IntervalSeq(Interval(windowStart, instant.toInstant))
        // No repetition
        case Some(TimingWindowEnd.After(duration, None))                                          =>
          IntervalSeq(Interval(windowStart, windowStart.plus(duration.toDuration)))
        // Repeat period n times
        case Some(TimingWindowEnd.After(duration, Some(TimingWindowRepeat(period, Some(times))))) =>
          val nearestStart = windowStartForPeriod(period)
          intervalsForStarts(
            List.unfold((0, nearestStart))(
              _.some
                .filter(_._1 <= times.value)
                .filter(_._2 <= within.upper)
                .map((iter, start) => (start, (iter + 1, start.plus(period.toDuration))))
            ),
            duration.toDuration
          )
        // Repeat period for ever
        case Some(TimingWindowEnd.After(duration, Some(TimingWindowRepeat(period, None))))        =>
          val nearestStart = windowStartForPeriod(period)
          intervalsForStarts(
            List.unfold(nearestStart)(
              _.some
                .filter(i => i <= within.upper)
                .map(start => (start, start.plus(period.toDuration)))
            ),
            duration.toDuration
          )
      }

    intervals & IntervalSeq(within)
  }

end TimingWindow

object TimingWindow:
  val inclusion: Lens[TimingWindow, TimingWindowInclusion] = Focus[TimingWindow](_.inclusion)
  val start: Lens[TimingWindow, Timestamp]                 = Focus[TimingWindow](_.start)
  val end: Lens[TimingWindow, Option[TimingWindowEnd]]     = Focus[TimingWindow](_.end)

  given Order[TimingWindow] = Order.by(tw =>
    (tw.start,
     tw.duration.getOrElse(TimeSpan.Max),
     tw.end
       .flatMap(TimingWindowEnd.after.getOption)
       .flatMap(_.repeat)
       .map(repeat => (repeat.period, repeat.times.map(_.value).orEmpty)),
     tw.inclusion
    )
  )

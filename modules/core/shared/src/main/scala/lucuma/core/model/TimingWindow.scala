// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.TimingWindowInclusion
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.refined.*
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism
import org.typelevel.cats.time.given

import java.time.Duration

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
  case At(endAt: Timestamp)                                          extends TimingWindowEnd
  case After(endAfter: Duration, repeat: Option[TimingWindowRepeat]) extends TimingWindowEnd

  val at: Prism[TimingWindowEnd, At]       = GenPrism[TimingWindowEnd, At]
  val times: Prism[TimingWindowEnd, After] = GenPrism[TimingWindowEnd, After]

/** Timing window definition. * */
final case class TimingWindow(
  inclusion: TimingWindowInclusion,
  start:     Timestamp,
  end:       Option[TimingWindowEnd]
) derives Eq

object TimingWindow:
  val inclusion: Lens[TimingWindow, TimingWindowInclusion] = Focus[TimingWindow](_.inclusion)
  val start: Lens[TimingWindow, Timestamp]                 = Focus[TimingWindow](_.start)
  val end: Lens[TimingWindow, Option[TimingWindowEnd]]     = Focus[TimingWindow](_.end)

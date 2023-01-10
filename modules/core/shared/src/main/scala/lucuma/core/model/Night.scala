// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.enums.Site
import lucuma.core.syntax.time._
import org.typelevel.cats.time._
import spire.math.Bounded

import java.time.Duration
import java.time.Instant

/**
 * Description of the start/end times for a night according to some criterion.
 * For example, the official observing night begins and ends at 2PM local time
 * while twilight bounded nights start and end according to defined angles of
 * the sun below the horizon.
 */
trait Night {

  /** Location at which the times described by this night are valid. */
  def site: Site

  def interval: Bounded[Instant]

  /** Start instant of the night (inclusive). */
  lazy val start: Instant = interval.lower

  /** End instant of the night (exclusive). */
  lazy val end: Instant = interval.upper

  /** Duration of the night. */
  lazy val duration: Duration = interval.duration

  /** Returns `true` if the night includes the given `Instant`. */
  def includes(instant: Instant): Boolean = interval.contains(instant)
}

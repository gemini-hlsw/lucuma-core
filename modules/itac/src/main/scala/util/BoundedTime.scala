// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.util
import cats.syntax.all.*
import lucuma.core.util.TimeSpan

/**
 * A class that represents a limited amount of time and the current amount used.
 * Contains methods for reserving and releasing used time amounts.  Both the
 * limit and the used amount must be positive time amounts.
 *
 * <p>The class allows more time to be used than available, if desired, as a
 * way to represent situations which are flexible about limit.  The client
 * can always enforce a hard limit by limiting itself to methods that do not
 * permit more time than available to be reserved.
 */
final case class BoundedTime(limit: TimeSpan, used: TimeSpan = TimeSpan.Zero) {

  /**
   * Returns true iff the amount of time used is more or equal to the limit.
   */
  def isFull = used >= limit

  /**
   * Returns the amount of time remaining (or amount of time overbooked if
   * negative).
   */
  def remaining = 
    if limit < used then TimeSpan.Zero else limit -| used

  /**
   * Returns the percentage of available time that has been used.  This will
   * be greater than 100 if overbooked.
   */
  def fillPercent: Double =
    100 * (if (limit.toMilliseconds == 0) 1.0 else used.toMilliseconds.toDouble/limit.toMilliseconds.toDouble)

  /**
   * Reserves the given time amount into a new BoundedTime instance together
   * with the current amount of time in this instance.  If there isn't enough
   * remaining space (or if releasing more time than used), None is returned.
   */
  def reserve(time: TimeSpan): Option[BoundedTime] =
    if (time == TimeSpan.Zero)
      Some(this)
    else {
      val tmp = used +| time
      if (tmp > limit)
        None
      else
        if (tmp < TimeSpan.Zero) None else Some(BoundedTime(limit, tmp))
    }

  /**
   * Reserves all the given time together with the amount of time in this
   * instance into a new BoundedTime instance (up to the limit).  Any left
   * over time is spilled into the result.  For example,
   *
   * <code>
   * val (newBoundedTime, leftOverTime) = boundedTime.reserveAvailable(...)
   * </code>
   */
  def reserveAvailable(time: TimeSpan): (BoundedTime, TimeSpan) = {
    val tmp = used +| time
    val (newUsed, rem) = if (tmp > limit)
      (limit, tmp -| limit)
    else if (tmp < TimeSpan.Zero)
      (TimeSpan.Zero, tmp)
    else
      (tmp, TimeSpan.Zero)

    if (newUsed == used) (this, rem) else (BoundedTime(limit, newUsed), rem)
  }

}
// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.util

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
final case class BoundedTime(limit: Time, used: Time = Time.Zero) {
  require((used >= Time.Zero) && (limit >= Time.Zero))

  /**
   * Returns true iff the amount of time used is more or equal to the limit.
   */
  def isFull = used >= limit

  /**
   * Returns true iff the amount of used time is 0.
   */
  def isEmpty = used.ms == 0

  /**
   * Returns true if more time has been used than is available.
   */
  def isOverbooked = used > limit


  /**
   * Returns the amount of time remaining (or amount of time overbooked if
   * negative).
   */
  def remaining = limit - used

  /**
   * Returns the percentage of available time that has been used.  This will
   * be greater than 100 if overbooked.
   */
  def fillPercent: Double =
    100 * (if (limit.ms == 0) 1.0 else used.to(limit.unit).value/limit.value)

//    val res = 100.0 * used.to(limit.unit).value/limit.value
//    if (java.lang.Double.isNaN(res)) 100.0 else res

  /**
   * Returns a new BoundedTime with the same limit as this one, but no used
   * time.  If already empty, returns this.
   */
  def empty = if (isEmpty) this else BoundedTime(limit)

  /**
   * If not already full, returns a new BoundedTime with the used time equal
   * to the limit.  If full or overbooked, returns this.
   */
  def fill  = if (isFull) this else fillExact

  /**
   * Returns a new BoundedTime in which the used time amount has been set to
   * equal the limit.  This will throw away any overbooked amount.
   */
  def fillExact = if (used == limit) this else BoundedTime(limit, limit)

  private def doReserve(time: Time, overbook: Boolean): Option[BoundedTime] =
    if (time == Time.Zero)
      Some(this)
    else {
      val tmp = used + time
      if ((tmp > limit) && !overbook)
        None
      else
        if (tmp < Time.Zero) None else Some(BoundedTime(limit, tmp))
    }


  /**
   * Reserves the given time amount into a new BoundedTime instance together
   * with the current amount of time in this instance.  If there isn't enough
   * remaining space (or if releasing more time than used), None is returned.
   */
  def reserve(time: Time): Option[BoundedTime] = doReserve(time, false)

  /**
   * Reserves the given time amount, returning a new BoundedTime instance.
   * Differs from the reserve method in that reservations beyond the limit
   * are permitted. Releasing more time than reserved still results in a None.
   */
  def overbook(time: Time): Option[BoundedTime] = doReserve(time, true)

  /**
   *  Releases the given time amount.  Equivalent to
   * <code>reserve(-time)</code>
   */
  def release(time: Time): Option[BoundedTime] = reserve(-time)

  /**
   * Reserves all the given time together with the amount of time in this
   * instance into a new BoundedTime instance (up to the limit).  Any left
   * over time is spilled into the result.  For example,
   *
   * <code>
   * val (newBoundedTime, leftOverTime) = boundedTime.reserveAvailable(...)
   * </code>
   */
  def reserveAvailable(time: Time): (BoundedTime, Time) = {
    val tmp = used + time
    val (newUsed, rem) = if (tmp > limit)
      (limit, tmp - limit)
    else if (tmp < Time.Zero)
      (Time.Zero, tmp)
    else
      (tmp, Time.Zero)

    if (newUsed == used) (this, rem) else (BoundedTime(limit, newUsed), rem)
  }

  /**
   * Releases as much of the given time amount as possible (but never more than
   * would take to leave the BoundedTime empty).  This is equivalent to
   * <code>reserveAvailable(-time)</code>
   */
  def releaseAvailable(time: Time): (BoundedTime, Time) = reserveAvailable(-time)

}
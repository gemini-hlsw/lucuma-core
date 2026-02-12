// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config


object DecRanged {
  def apply[T](startDeg: Int, endDeg: Int, binValue: T): DecRanged[T] =
    new DecRanged[T](DecRange(startDeg, endDeg), binValue)

  def inclusive[T](start: Int, end: Int, binValue: T): DecRanged[T] =
    DecRanged(DecRange.inclusive(start, end), binValue)

  /**
   * Validates that a sequence of DecRanged is sorted, non-overlapping, and
   * abutting.
   */
  def validate[T](bins: Seq[DecRanged[T]]): Boolean =
    DecRange.validate(bins.map(_.range))

}

/**
 * A grouping of a range of declinations with a parameterized type.
 */
final case class DecRanged[T](range: DecRange, binValue: T) {

  def inclusive: DecRanged[T] =
    if (range.isInclusive) this else DecRanged(range.inclusive, binValue)

  def map[U](f: T => U): DecRanged[U] = DecRanged(range, f(binValue))
  def updated(newValue: T): DecRanged[T] = DecRanged[T](range, newValue)
}

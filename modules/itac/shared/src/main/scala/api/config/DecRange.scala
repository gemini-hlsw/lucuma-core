// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config


import edu.gemini.tac.qengine.p1.Target
import edu.gemini.tac.qengine.util.Angle

/**
 * Defines a range of Declinations (interpreted as degrees).  DecRanges include
 * the start of the range and exclude the end by default.  An inclusive
 * DecRange option is available to account for the final range (eg., (80, 90),
 * if necessary.
 */
class DecRange(val startDeg: Int, val endDeg: Int) {
  require(startDeg <= endDeg)

  def isInclusive = false

  /**
   * Creates an inclusive DecRange using this range's start and end.
   */
  def inclusive: DecRange = DecRange.inclusive(startDeg, endDeg)

  def contains(dec: Angle): Boolean = {
    val dint = dec.toDeg.mag.toInt
    dint >= startDeg && dint < endDeg
  }

  def contains(t: Target): Boolean = contains(t.dec)

  /**
   * Returns true if this DecRange falls immediately to the right of the
   * given DecRange with no overlapping or gaps between.  For example,
   * <code>(0, 10] abuts (10, 20]</code>.
   */
  def abutsRight(that: DecRange): Boolean = {
    startDeg < that.startDeg && endDeg == that.startDeg
  }

  override def equals(other: Any) = other match {
    case that: DecRange =>
      startDeg == that.startDeg && endDeg == that.endDeg &&
      isInclusive == that.isInclusive
    case _ => false
  }

  override def hashCode: Int = {
    41 * (41 * (41 + startDeg) + endDeg) + isInclusive.hashCode
  }

  override def toString: String = "(%d, %d]".format(startDeg, endDeg)

}

object DecRange {

  def apply(startDeg: Int, endDeg: Int): DecRange = new DecRange(startDeg, endDeg)

  /**
   * A DecRange that includes the ending range of Decs.  By default a DecRange
   * is defined as the range of Decs <code>(start, end]</code>, but we need a
   * way to specify a range that ends with (and contains) 90 degrees.
   */
  class Inclusive(startDeg: Int, endDeg: Int) extends DecRange(startDeg, endDeg) {
    override def isInclusive = true
    override def inclusive = this
    override def contains(dec: Angle): Boolean = {
      super.contains(dec) || dec.toDeg.mag == endDeg
    }
    override def abutsRight(that: DecRange) = false
    override def toString: String = "(%d, %d)".format(startDeg, endDeg)
  }

  def inclusive(start: Int, end: Int): DecRange = new Inclusive(start, end)

  private def validate(h: DecRange, t: Seq[DecRange]): Boolean = t match {
      case Seq() => true
      case Seq(n, tail @ _*) => h.abutsRight(n) && validate(n, tail)
    }

  /**
   * Validates that a sequence of DecRange is sorted, non-overlapping, and
   * abutting.
   */
  def validate(ranges: Seq[DecRange]): Boolean = ranges match {
      case Seq() => true
      case Seq(h, tail @ _*) => validate(h, tail)
    }



}

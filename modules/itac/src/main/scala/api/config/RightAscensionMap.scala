// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.tac.qengine.p1.Target
import lucuma.core.math.HourAngle
import lucuma.core.math.RightAscension


object RightAscensionMap {

  // The total number of minutes in the full range of RAs.
  val TotalMin = 24*60

  def apply[T](bins: Seq[T]): RightAscensionMap[T] = new RightAscensionMap[T](bins.toIndexedSeq)

  /**
   * Generates an RightAscensionMap given a bin size in minutes, which must evenly
   * divide the total number of minutes in 24 hours, and a function that
   * calculates a value based upon the angle corresponding to the center of
   * the bin and the size of the bin in minutes.
   */
  def gen[T ](binSizeMin: Int)(f: (RightAscension, Int) => T): RightAscensionMap[T] = {
    require((TotalMin % binSizeMin) == 0)

    val r = 0 until TotalMin by binSizeMin
    val halfSize = binSizeMin/2.0
    new RightAscensionMap(r.map(min => f(RightAscension(HourAngle.fromDoubleMinutes(min+halfSize)), binSizeMin)))
  }

  def gen15MinBins[T ] = gen[T](15)
  def gen30MinBins[T ] = gen[T](30)
  def gen45MinBins[T ] = gen[T](45)
  def gen1HrBins[T ]   = gen[T](60)
  def gen90MinBins[T ] = gen[T](90)
  def gen2HrBins[T ]   = gen[T](120)
  def gen3HrBins[T ]   = gen[T](180)
  def gen4HrBins[T ]   = gen[T](240)
}

import RightAscensionMap._

// Choosing an IndexedSeq here because we want fast random access and arrays
// are complicated.

/**
 * An RightAscensionMap is a parametrized collection indexed by RA angle.
 */
case class RightAscensionMap[T] private (val bins: IndexedSeq[T]) {
  require((TotalMin % bins.length) == 0)

  /** Size of each bin in minutes. */
  val sizeMin = TotalMin / bins.length

  def updated(ra: RightAscension, f: T => Option[T]): Option[RightAscensionMap[T]] = {
    val i = indexOf(ra)
    f(bins(i)).map(bins.updated(i, _)).map(new RightAscensionMap(_))
  }

  def updated(ra: RightAscension, t: T): RightAscensionMap[T] = {
    new RightAscensionMap(bins.updated(indexOf(ra), t))
  }

  def indexOf(ra: RightAscension): Int = ra.toHourAngle.toDoubleMinutes.toInt / sizeMin

  def apply(min: Int): T  = bins((min % TotalMin) / sizeMin)
  def apply(ra: RightAscension): T  = bins(indexOf(ra))
  def apply(t: Target): T = apply(t.ra)

  def map[U](f: T => U): RightAscensionMap[U] = new RightAscensionMap[U](bins.map(f))
}


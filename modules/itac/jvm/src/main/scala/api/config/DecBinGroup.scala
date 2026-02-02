// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.tac.qengine.util.Angle
import edu.gemini.tac.qengine.p1.Target

object DecBinGroup {
  val TotalDeg = 180

  def fromBins[T](bins: DecBin[T]*): DecBinGroup[T] = {
    require(DecRange.validate(bins.map(_.range)))
    new DecBinGroup(bins.toIndexedSeq)
  }

  def ranges(binSizeDeg: Int): IndexedSeq[DecRange] = {
    require((TotalDeg % binSizeDeg) == 0)
    val r = (-90 until 90 by binSizeDeg).map(deg => DecRange(deg, deg+binSizeDeg))
    r.init :+ r.last.inclusive
  }

  def apply[T](vals: Seq[T]): DecBinGroup[T] =
    new DecBinGroup(for (v <- ranges(TotalDeg/vals.size).zip(vals)) yield DecBin(v._1, v._2))

  def gen[T](binSizeDeg: Int)(f: DecRange => Option[T]): DecBinGroup[T] = {
    require((TotalDeg % binSizeDeg) == 0)
    new DecBinGroup(ranges(binSizeDeg).collect(range => f(range) match {
      case Some(t) => DecBin(range, t)
    }))
  }

  def gen5DegBins[T]  = gen[T](5)
  def gen10DegBins[T] = gen[T](10)
  def gen15DegBins[T] = gen[T](15)
  def gen20DegBins[T] = gen[T](20)
  def gen30DegBins[T] = gen[T](30)
  def gen45DegBins[T] = gen[T](45)
}

/**
 * A collection of DecBins, which must be listed in order by DecRange and must
 * abut.
 */
case class DecBinGroup[T] private (val bins: IndexedSeq[DecBin[T]]) {

  private def updated(i: Int, bin: DecBin[T], f: T => Option[T]): Option[DecBinGroup[T]] = {
    f(bin.binValue).map(bv => new DecBinGroup(bins.updated(i, new DecBin(bin.range, bv))))
  }

  def updated(dec: Angle, f: T => Option[T]): Option[DecBinGroup[T]] = indexOf(dec) match {
    case i if i < 0 => None
    case i => updated(i, bins(i), f)
  }

  def updated(dec: Angle, t: T): Option[DecBinGroup[T]] = indexOf(dec) match {
    case i if i < 0 => None
    case i => Some(new DecBinGroup(bins.updated(i, DecBin(bins(i).range, t))))
  }

  def indexOf(dec: Angle): Int = bins.indexWhere(_.range.contains(dec))

  def get(dec: Angle): Option[DecBin[T]] = {
    bins.find(_.range.contains(dec))
  }
  def get(t: Target): Option[DecBin[T]] = get(t.dec)

  def map[U](f: T => U): DecBinGroup[U] = {
    new DecBinGroup[U](bins.map(bin => DecBin[U](bin.range, f(bin.binValue))))
  }

}

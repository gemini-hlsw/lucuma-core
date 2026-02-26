// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qengine.api.config

import edu.gemini.tac.qengine.p1.ItacTarget
import lucuma.core.math.Declination

object DeclinationMap {
  val TotalDeg = 180

  def fromBins[T](bins: DecRanged[T]*): DeclinationMap[T] = {
    require(DecRange.validate(bins.map(_.range)))
    new DeclinationMap(bins.toIndexedSeq)
  }

  def ranges(binSizeDeg: Int): IndexedSeq[DecRange] = {
    require((TotalDeg % binSizeDeg) == 0)
    val r = (-90 until 90 by binSizeDeg).map(deg => DecRange(deg, deg+binSizeDeg))
    r.init :+ r.last.inclusive
  }

  def apply[T](vals: Seq[T]): DeclinationMap[T] =
    new DeclinationMap(for (v <- ranges(TotalDeg/vals.size).zip(vals)) yield DecRanged(v._1, v._2))

  def gen[T](binSizeDeg: Int)(f: DecRange => Option[T]): DeclinationMap[T] = {
    require((TotalDeg % binSizeDeg) == 0)
    new DeclinationMap(ranges(binSizeDeg).collect(range => f(range) match {
      case Some(t) => DecRanged(range, t)
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
case class DeclinationMap[T] private (val bins: IndexedSeq[DecRanged[T]]) {

  private def updated(i: Int, bin: DecRanged[T], f: T => Option[T]): Option[DeclinationMap[T]] = {
    f(bin.binValue).map(bv => new DeclinationMap(bins.updated(i, new DecRanged(bin.range, bv))))
  }

  def updated(dec: Declination, f: T => Option[T]): Option[DeclinationMap[T]] = indexOf(dec) match {
    case i if i < 0 => None
    case i => updated(i, bins(i), f)
  }

  def updated(dec: Declination, t: T): Option[DeclinationMap[T]] = indexOf(dec) match {
    case i if i < 0 => None
    case i => Some(new DeclinationMap(bins.updated(i, DecRanged(bins(i).range, t))))
  }

  def indexOf(dec: Declination): Int = bins.indexWhere(_.range.contains(dec))

  def get(dec: Declination): Option[DecRanged[T]] = {
    bins.find(_.range.contains(dec))
  }
  def get(t: ItacTarget): Option[DecRanged[T]] = get(t.dec)

  def map[U](f: T => U): DeclinationMap[U] = {
    new DeclinationMap[U](bins.map(bin => DecRanged[U](bin.range, f(bin.binValue))))
  }

}

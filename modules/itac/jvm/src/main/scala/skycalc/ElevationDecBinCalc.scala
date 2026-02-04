// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.qengine.skycalc;

import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.math.skycalc.solver.Samples
import lucuma.core.math.BoundedInterval
import spire.math.extras.interval.IntervalSeq
import java.time.Instant
import lucuma.core.math.skycalc.solver.AirmassSolver
import lucuma.core.math.skycalc.ImprovedSkyCalc
import lucuma.core.math.RightAscension
import edu.gemini.tac.qengine.util.Percent
import spire.math.interval.Closed
import spire.math.interval.Open
import java.time.ZonedDateTime

/**
 * Calculates dec bin percentages based upon airmass elevation constraints.
 */
class ElevationDecBinCalc(conf: ElevationConfig) extends DecBinCalc {

  def getSolvers(site: Site, coords: List[Coordinates]): List[BoundedInterval[Instant] => IntervalSeq[Instant]] =
    coords.map: cs => 
      bi => 
        val samples = 
          Samples.atFixedRate(bi, java.time.Duration.ofMinutes(1)): i =>
            ImprovedSkyCalc(site.place).calculate(cs, i, false)
        AirmassSolver(conf.minAirmass, conf.maxAirmass).solve(samples)(bi)

  def calc(site: Site, start: ZonedDateTime, end: ZonedDateTime, size: DecBinSize, ra: RightAscension): List[Percent] =
    calc(site, start.toInstant(), end.toInstant(), size, ra)

  def calc(site: Site, start: Instant, end: Instant, size: DecBinSize, ra: RightAscension): List[Percent] = {
    val coords  = size.genCoordinates(ra)
    val solvers = getSolvers(site, coords)
    val totals  = Array.ofDim[Long](size.binCount)
    val it      = NightIterator.bounded(conf.bounds, site, start, end)
    while (it.hasNext) {
      var bin = 0
      val night = it.next
      solvers.foreach { s =>
        val u = s.apply(night.interval)
        u.intervalIterator.foreach: i =>
          
          val end = i.upperBound match
            case Closed(a) => a
            case Open(a) => a
            case b => sys.error(s"can't handle $b")
          
          val start = i.lowerBound match
            case Closed(a) => a
            case Open(a) => a
            case b => sys.error(s"can't handle $b")

          totals(bin) += end.toEpochMilli() - start.toEpochMilli()
          
          bin += 1
      }
    }    
    val max = totals(ElevationDecBinCalc.getSiteIndex(site, size))
    totals.toList.map: cur =>
      Percent(100.0 * cur.toDouble / max)
  }

}

object ElevationDecBinCalc:

    // Gets the index of the bin that corresponds to the site.  All percentage
    // calculations are relative to this bin.
    private def getSiteIndex(site: Site, size: DecBinSize): Int =
      math.floor(site.latitude.toAngle.toSignedDoubleDegrees + 90.0).toInt/ size.degrees

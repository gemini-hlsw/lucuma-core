// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

/*
 * Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
 * For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause
 */

//
// $
//

package edu.gemini.qengine.skycalc;

import lucuma.core.enums.Site
import lucuma.core.enums.TwilightType
import lucuma.core.math.RightAscension
import lucuma.core.model.IntCentiPercentUnbounded
import lucuma.core.model.Semester

import java.time.Instant
import java.time.ZonedDateTime


/**
 * Calculated RA bin times and Dec bin percentages.
 */
final case class RaDecBinCalc(raHours: List[Hours], decPercents: List[IntCentiPercentUnbounded])

object RaDecBinCalc {

  val RA_CALC  = ExcelRaBinCalc(TwilightType.Nautical)         // Using the Excel method since that most closely tracks what they expect.
  val DEC_CALC = ElevationDecBinCalc(ElevationConfig.DEFAULT)  // Using elevation method since I don't have another way.

  // we assume raSize.genRas and hrs are the same length and non-empty
  private def max(raSize: RaBinSize, hrs: List[Hours]): (RightAscension, Hours) =
    (raSize.genRas.zip(hrs)).maxBy(_._2.hours)

  /**
   * Calculates the RA/Dec bin and returns the result.
   *
   * @param site Gemini site to which the calculation corresponds
   * @param semester semester in which the calculation is valid
   * @param raSize size of the ra bins in minutes
   * @param decSize size of the dec bins in degrees
   *
   * @return RA/Dec bin calculation
   */
  def calc(site: Site, start: Instant, end: Instant, raSize: RaBinSize, decSize: DecBinSize): RaDecBinCalc = {
    val raHours = RA_CALC.calc(site, start, end, raSize);
    val (ra, hrs) = max(raSize, raHours);
    val decPercents = DEC_CALC.calc(site, start, end, decSize, ra);
    RaDecBinCalc(raHours, decPercents);
  }

  def calc(site: Site, start: ZonedDateTime, end: ZonedDateTime, raSize: RaBinSize, decSize: DecBinSize): RaDecBinCalc =
    calc(site, start.toInstant(), end.toInstant(), raSize, decSize)

  final case class Key(site: Site, semester: Semester, raBinSize: RaBinSize, decBinSize: DecBinSize)

  val CACHE_SIZE  = 50
  val LOAD_FACTOR = 0.75f

  val cacheCapacity = math.round(math.ceil((CACHE_SIZE + 1) / LOAD_FACTOR)).toInt

  val CACHE = new java.util.LinkedHashMap[Key, RaDecBinCalc](cacheCapacity, LOAD_FACTOR, true):
    override def removeEldestEntry(me: java.util.Map.Entry[Key, RaDecBinCalc]): Boolean =
      size() > CACHE_SIZE

  /**
   * Obtains the RA/Dec bin calculation corresponding to the given site,
   * semester, and RA/Dec bin sizes.  If the value has been calculated
   * previously and is still cached, it returns immediately with the result.
   * Otherwise, it performs the calculation and caches the answer.
   *
   * @param site Gemini site to which the calculation corresponds
   * @param semester semester in which the calculation is valid
   * @param raSize size of the ra bins in minutes
   * @param decSize size of the dec bins in degrees
   *
   * @return RA/Dec bin calculation
   */
  def get(site: Site, semester: Semester, raSize: RaBinSize, decSize: DecBinSize): RaDecBinCalc =
    val key = Key(site, semester, raSize, decSize)
    var res: RaDecBinCalc = null
    CACHE.synchronized(CACHE.get(key))
    if res == null then
      res = calc(site, semester.start.atSite(site), semester.end.atSite(site), raSize, decSize)
      CACHE.synchronized(CACHE.put(key, res): Unit)
    res
    
}
// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.qengine.skycalc;

import lucuma.core.enums.Half
import lucuma.core.enums.Site
import lucuma.core.enums.TwilightType
import lucuma.core.math.skycalc.ImprovedSkyCalc
import lucuma.core.model.Semester
import lucuma.core.model.Semester.YearInt

import java.time.Instant
import java.time.ZonedDateTime

/**
 * Calculates RaBin times based upon the method extracted from the
 * "RAdistributions.xls" spreadsheet used in the ITAC process prior to the
 * introduction of the automatted system.
 */
class ExcelRaBinCalc(bounds: TwilightType) extends RaBinCalc {

  def calc(site: Site, start: ZonedDateTime, end: ZonedDateTime, size: RaBinSize): List[Hours] =
    calc(site, start.toInstant(), end.toInstant(), size)

  def calc(site: Site, start: Instant, end: Instant, size: RaBinSize): List[Hours] = {

    val binCount   = size.binCount
    val totals     = Array.ofDim[Long](binCount)
    val binSizeMs  = size.arcMinutes.toLong * 60 * 1000; // convert from min to ms
    val ras        = size.genRas

    NightIterator.bounded(bounds, site, start, end).foreach { night =>
      val eve  = ImprovedSkyCalc(site.place).getSiderealTime(night.start)
      val morn = ImprovedSkyCalc(site.place).getSiderealTime(night.end)
      (0 until binCount).foreach { bin =>
        val ra = ras(bin).toHourAngle.toDoubleHours
        if (eve < morn) {
            if (ra>eve && ra<morn) totals(bin) += binSizeMs;
        } else if (morn<ra) {
            if (eve<ra) totals(bin) += binSizeMs;
        } else {
            if (morn>ra) totals(bin) += binSizeMs;
        }
      }
    }

    totals.toList.map(Hours.fromMillisec)

  }

 
}

object ExcelRaBinCalc:
  @main def main: Unit =
    val sz = RaBinSize.ofArcMinutes(60).get
    val site = Site.GS;
    val sem = Semester(YearInt.unsafeFrom(2020), Half.A)
    val start = sem.start.atSite(site).toInstant
    val end = sem.end.atSite(site).toInstant
    val hrs = ExcelRaBinCalc(TwilightType.Nautical).calc(site, start, end, sz);
    sz.genRas.zip(hrs).foreach: 
      case (ra, hrs) => println(s"$ra $hrs")

  
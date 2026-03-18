// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.tac.qservice.impl.shutdown

import edu.gemini.qengine.skycalc.RaBinSize
import edu.gemini.qengine.skycalc.RaDecBinCalc
import edu.gemini.tac.qengine.api.config.Shutdown
import lucuma.core.enums.Site
import lucuma.core.model.Semester
import lucuma.core.util.TimeSpan

/**
 *
 */
object ShutdownCalc {

  def timePerRa(s: Shutdown, size: RaBinSize): List[TimeSpan] =
    RaDecBinCalc.RA_CALC.calc(s.site, s.start.toInstant(), s.end.toInstant(), size).map {
      hrs => TimeSpan.fromHoursBounded(hrs.hours)
    }

  def asTime(s: Shutdown, size: RaBinSize): TimeSpan =
    TimeSpan.fromHoursBounded(timePerRa(s, size).map(_.toHours.toDouble).sum)

  def trim(s: Shutdown, site: Site, semester: Semester): Option[Shutdown] = {
    val semStart = semester.start.atSite(site)
    val semEnd   = semester.end.atSite(site)

    if ((site != s.site) || (s.end.compareTo(semStart) <= 0) || (s.start.compareTo(semEnd) >= 0)) None
    else {
      val newStart = if (s.start.compareTo(semStart) < 0) semStart else s.start
      val newEnd   = if (semEnd.compareTo(s.end) < 0) semEnd else s.end

      if ((s.start == newStart) && (s.end == newEnd)) Some(s)
      else Some(Shutdown(s.site, newStart, newEnd))
    }
  }

  def trim(sds: List[Shutdown], site: Site, semester: Semester): List[Shutdown] =
    sds.flatMap(trim(_, site, semester))

  /**
   * Combines visible hours per RA for all the given shutdown periods into a
   * single list.
   */
  def sumHoursPerRa(shutdowns: List[Shutdown], size: RaBinSize): List[TimeSpan] = {
    val lsts  = shutdowns.map { timePerRa(_, size) }
    val zeros = List.fill(size.binCount)(TimeSpan.Zero)

    lsts.foldLeft(zeros) {
      (sum, cur) => sum.zip(cur).map { case (t1, t2) => t1 +| t2 }
    }
  }

  /**
   * Validates the collection of Shutdown, making sure that the dates for the
   * same sites don't overlap.
   */
  def validate(shutdowns: List[Shutdown]): Boolean = {
    val (north, south) = shutdowns.partition(_.site == Site.GN)

    def validateDates(lst: List[Shutdown]): Boolean = {
      lst.sorted match {
        case Nil          => true
        case head :: tail =>
          val (_, res) = tail.foldLeft((head,true)) {
            case ((last, result), cur) =>
              (cur, result && (last.end.isBefore(cur.start) || last.end.equals(cur.start)))
          }
          res
      }
    }

    validateDates(north) && validateDates(south)
  }
}

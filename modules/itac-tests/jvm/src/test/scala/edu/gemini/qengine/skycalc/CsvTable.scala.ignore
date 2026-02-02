// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.qengine.skycalc

import edu.gemini.spModel.core.{Semester, Site}

import collection.JavaConverters._
import edu.gemini.skycalc.Angle

/**
 * Writes out a CSV table with the RA/Dec bin sizes.
 */
object CsvTable {

  def hrs(site: Site, semester: Semester): List[Hours] = {
    val sz   = new RaBinSize(60)
    val calc = new ExcelRaBinCalc
    calc.calc(site, semester.getStartDate(site), semester.getEndDate(site), sz).asScala.toList
  }

  def percs(site: Site, semester: Semester, ra: Angle): List[Percent] = {
    val sz   = new DecBinSize(20)
    val calc = new ElevationDecBinCalc()
    calc.calc(site, semester.getStartDate(site), semester.getEndDate(site), sz, ra).asScala.toList
  }

  def table(site: Site, semester: Semester) = {

    val hList = hrs(site, semester)
    val buf   = Array.ofDim[Double](9, 24)

    val percList = for {
      (hr, ra) <- hList.zipWithIndex
    } yield percs(site, semester, new Angle(ra + 0.5, Angle.Unit.HOURS)) map { perc =>
      hr.getHours * perc.getAmount / 100.0
    }

    for (col <- 0 to 23; row <- 0 to 8) { buf(row)(col) = percList(col)(row) }

    val hdr1 = hList.zipWithIndex map {
      case (_, index) => "%d".format(index)
    }
    val hdr2 = hList.zipWithIndex map {
      case (hrs, index) => "%.1f".format(hrs.getHours)
    }

    def rowData(row: Int): List[String] =
      (for {
        col <- 0 to 23
      } yield "%5.1f".format(buf(row)(col))).toList

    val rows = for {
      row  <- 8 to 0 by -1
      dec2 = 90 + (row - 8) * 20
      dec1 = dec2 - 20
    } yield "(%d to %d)".format(dec1, dec2) :: rowData(row)

    println(("" :: hdr1).mkString(","))
    println(("" :: hdr2).mkString(","))
    rows foreach { row =>
      println(row.mkString(","))
    }
  }

  def main(args: Array[String]) = {
    Site.values foreach { site =>
      List("2011A", "2011B") foreach { semStr =>
        val semester = Semester.parse(semStr)
        val start    = semester.getStartDate(site)
        val end      = semester.getEndDate(site)
        println("\n%s %s".format(site.abbreviation, semStr))
        val raCalc = new ExcelRaBinCalc()
        val hrs    = raCalc.calc(site, start, end, new RaBinSize(60)).asScala.toList
        hrs.zipWithIndex foreach {
          case (hr, i) => println("%2d %5.1f".format(i, hr.getHours))
        }
      }
    }
//    table(Site.GN, Semester.parse("2011A"))

    /*
    val airmassList = List(1.7, 1.8, 1.9, 2.0, 2.1, 2.15, 2.2, 2.3, 2.4, 2.5)
    val res = for {
      airmass <- airmassList
    } yield (airmass, new ElevationDecBinCalc(new ElevationConfig(TwilightBoundType.NAUTICAL, 1.0, airmass)))

    res foreach {
      case (airmass, calc) =>
        println("\nMax Airmass: %.2f".format(airmass))
        val labels = for {
          row <- 8 to 0 by -1
          dec2 = 90 + (row-8)*20
          dec1 = dec2 - 20
        } yield "(%3d, %3d)".format(dec1,dec2)

        val ps = calc.calc(Site.GN, Semester.parse("2011A"), new DecBinSize(20), new Angle(14.5, Angle.Unit.HOURS)).asScala.toList
        labels.zip(ps.reverse) foreach {
          case (label, p) => println("%12s %3d".format(label, p.getAmount.round))
        }
    }

    val raCalc = new ExcelRaBinCalc()
    val hrs    = raCalc.calc(Site.GN, Semester.parse("2011A"), new RaBinSize(60)).asScala.toList
    hrs.zipWithIndex foreach {
      case (hr, i) => println("%d %.2f".format(i, hr.getHours))
    }
     */

//    val c = new ElevationDecBinCalc(new ElevationConfig())
//    val percs = c.calc(Site.GN, Semester.parse("2011A"), new DecBinSize(20), new Angle(14.5, Angle.Unit.HOURS), null)
//    println(percs.asScala.mkString(", "))

//    val calc = RaDecBinCalc.calc(Site.GN, Semester.parse("2011A"), new RaBinSize(60), new DecBinSize(20))
//    println(calc.getDecPercentages.asScala.mkString(", "))
  }
}

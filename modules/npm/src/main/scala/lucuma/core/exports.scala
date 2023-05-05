// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import scala.scalajs.js
import cats.syntax.all.*
import scala.scalajs.js.annotation.JSExportTopLevel
import lucuma.core.enums.Site
import java.time.Instant
import lucuma.core.model.CoordinatesAtVizTime
import java.time.Duration
import lucuma.core.math.Coordinates
import lucuma.core.math.skycalc.SkyCalcResults
import lucuma.core.math.skycalc.ImprovedSkyCalc
import lucuma.core.model.ObservingNight
import java.time.LocalDate
import lucuma.core.enums.TwilightType
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.JSConverters.*

/**
 * JS facade to plot points calculated with the method `nightPlot`.
 */
@JSExportTopLevel("PlotPoint")
class PlotPoint(@JSExport val instant: js.Date, @JSExport val airmass: Double, @JSExport val altitude: Double) {
  override def toString(): String = s"PlotPoin($instant, $airmass, $altitude)"
}

object NightPlot:
  private val PlotEvery: Duration   = Duration.ofMinutes(1)

  /**
    * JS facade to call `nightPlot` from JavaScript.
    *
    *
    * @param site Site, either GS or GN
    * @param jsDate, reference date
    * @param coords, coordinates in HMS/DMS format
    * @return an Array of PlotPoint
    */
  @JSExportTopLevel("nightPlot")
  def jsNightPlot(
    site:   Site,
    start:  js.Date,
    coords: String
  ): js.Array[PlotPoint] =  {
    val year = start.getUTCFullYear()
    val month = start.getMonth() + 1 // Note: Month values in JavaScript are zero-based, so we need to add 1 to get the correct month value
    val day = start.getDate()

    val localDate = LocalDate.of(year.toInt, month.toInt, day.toInt)
    val coordsAtVizTime = Coordinates.fromHmsDms.getOption(coords).get
    nightPlot(site, localDate, CoordinatesAtVizTime(coordsAtVizTime)).map { case (i, p) =>
      new PlotPoint(new js.Date(i.toEpochMilli().toDouble), p.airmass, p.altitude.toAngle.toSignedDoubleDegrees)
    }.toJSArray
  }

  private def nightPlot(
    site:      Site,
    localDate: LocalDate,
    coords:    CoordinatesAtVizTime
  ): List[(Instant, SkyCalcResults)] =  {
    val observingNight  = ObservingNight.fromSiteAndLocalDate(site, localDate)
    val tbOfficialNight = observingNight.twilightBoundedUnsafe(TwilightType.Official)
    val tbNauticalNight = observingNight.twilightBoundedUnsafe(TwilightType.Nautical)

    val start          = tbOfficialNight.start
    val end            = tbOfficialNight.end
    forInterval(site, start, end, PlotEvery, _ => coords.value)
  }

  private def forInterval(
    site:             Site,
    start:            Instant,
    end:              Instant,
    every:            Duration,
    coordsForInstant: Instant => Coordinates
  ): List[(Instant, SkyCalcResults)] = {
    val calc     = ImprovedSkyCalc(site.place)
    val instants =
      List.unfold(start)(prev =>
        prev.plus(every).some.filter(_.isBefore(end)).map(i => (i, i))
      ) :+ end
    instants.map { i =>
      (i, calc.calculate(coordsForInstant(i), i, true))
    }
  }


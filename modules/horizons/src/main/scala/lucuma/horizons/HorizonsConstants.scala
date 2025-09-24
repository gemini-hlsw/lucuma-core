// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.horizons
import lucuma.core.enums.Site
import org.http4s.syntax.literals.uri

import java.time.ZoneOffset
import java.time.format.DateTimeFormatter
import java.util.Locale

object HorizonsConstants:

  val HorizonsUri                = uri"https://ssd.jpl.nasa.gov/api/horizons.api"
  val Command                    = "COMMAND"
  val Ephemeris                  = "MAKE_EPHEM"
  val Format                     = "format"
  val StartTime                  = "START_TIME"
  val StopTime                   = "STOP_TIME"
  val StepSize                   = "STEP_SIZE"
  val Center                     = "CENTER"
  val CenterCoord                = "coord"
  val CoordType                  = "COORD_TYPE"
  val CoordTypeGeo               = "GEODETIC"
  val SiteCoord                  = "SITE_COORD"
  val ExtraPrecision             = "extra_prec"
  val TimeDigits                 = "time_digits"
  val FractionalSec              = "FRACSEC"
  val Quantities                 = "QUANTITIES"
  val Yes                        = "YES"
  val No                         = "NO"
  val Text                       = "text"

  val HorizonsDateFormat = 
    DateTimeFormatter
      .ofPattern("yyyy-MMM-dd HH:mm:ss.SSS", Locale.US)
      .withZone(ZoneOffset.UTC)

  def horizonsCoordsAt(site: Site): String =
    String.format(
      "'%1.6f,%1.6f,%1.3f'",
      site.place.longitude.toDoubleDegrees,
      site.place.latitude.toAngle.toDoubleDegrees,
      site.place.altitude.value.value / 1000.0
    )

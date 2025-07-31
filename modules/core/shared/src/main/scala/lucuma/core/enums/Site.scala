// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import coulomb.*
import coulomb.units.si.Meter
import eu.timepit.refined.numeric.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.math.Angle
import lucuma.core.math.Declination
import lucuma.core.math.Lat
import lucuma.core.math.Lon
import lucuma.core.math.Place
import lucuma.core.math.units.*
import lucuma.core.model.AirMass
import lucuma.core.model.ObservingNight
import lucuma.core.util.DateInterval
import lucuma.core.util.Enumerated

import java.time.Duration
import java.time.Instant
import java.time.ZoneId

/**
  * Enumerated type for Gemini observing sites.
  */
enum Site(
  val tag:       String,
  val shortName: String,
  val longName:  String,
  val place:     Place,
  val mountain:  String
) derives Enumerated:
  val latitude: Lat                        = place.latitude
  val longitude: Lon                       = place.longitude
  val altitude: Quantity[NonNegInt, Meter] = place.altitude
  val timezone: ZoneId                     = place.timezone

  def midpoint(active: DateInterval): Instant =
    val start    = ObservingNight.fromSiteAndLocalDate(this, active.start).start
    val end      = ObservingNight.fromSiteAndLocalDate(this, active.end).end
    val duration = Duration.between(start, end)
    start.plus(duration.dividedBy(2L))

  /**
    * Minimum airmass that a given declination reaches from this site.
    */
  def minimumAirMassFor(dec: Declination): Option[AirMass] =
    AirMass.minimumFor(dec, latitude)

  case GN
      extends Site("GN",
                   "GN",
                   "Gemini North",
                   Place(
                     Lat.fromAngleWithCarry(Angle.fromDoubleDegrees(19.8238068))._1,
                     Lon.fromDoubleDegrees(-155.4690550),
                     4213.withRefinedUnit[NonNegative, Meter],
                     ZoneId.of("Pacific/Honolulu")
                   ),
                   "Mauna Kea"
      )

  case GS extends Site(
    "GS",
    "GS",
    "Gemini South",
    Place(
      Lat.fromAngleWithCarry(Angle.fromDoubleDegrees(-30.2407494))._1,
      Lon.fromDoubleDegrees(-70.7366867),
      2722.withRefinedUnit[NonNegative, Meter],
      ZoneId.of("America/Santiago")
    ),
    "Cerro Pachon"
  )

object Site:
  val all: List[Site] = List(GN, GS)

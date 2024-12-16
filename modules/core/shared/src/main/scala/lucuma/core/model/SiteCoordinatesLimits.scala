// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension

case class SiteCoordinatesLimits(
  raStart:  RightAscension,
  raEnd:    RightAscension,
  decStart: Declination,
  decEnd:   Declination
) derives Eq {
  def inRaLimits(ra: RightAscension): Boolean =
    if (raStart < raEnd)
      raStart <= ra && ra <= raEnd
    else
      raStart <= ra || ra <= raEnd

  def inDecLimits(dec: Declination): Boolean =
    val a = decStart.min(decEnd)
    val b = decStart.max(decEnd)
    a <= dec && dec <= b

  def inLimits(c: Coordinates): Boolean =
    inRaLimits(c.ra) && inDecLimits(c.dec)
}

case class CallCoordinatesLimits(
  north: SiteCoordinatesLimits,
  south: SiteCoordinatesLimits
) derives Eq:
  def siteLimits(at: Site): SiteCoordinatesLimits =
    at match
      case Site.GN => north
      case Site.GS => south


// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.arb

import gsp.math.{ Angle, Declination, Location }
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._

trait ArbLocation {
  import ArbAngle._
  import ArbDeclination._

  implicit val arbLocation: Arbitrary[Location] =
    Arbitrary {
      for {
        lat <- arbitrary[Declination]
        lon <- arbitrary[Angle]
        alt <- arbitrary[Double]
      } yield Location(lat, lon, alt)
    }

  implicit val cogCoordinates: Cogen[Location] =
    Cogen[(Declination, Angle, Double)].contramap(loc => (loc.latitude, loc.longitude, loc.altitude))
}

object ArbLocation extends ArbLocation

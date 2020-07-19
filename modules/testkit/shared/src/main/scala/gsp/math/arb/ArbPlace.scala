// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.arb

import gsp.math.{ Angle, Declination, Place }
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._

trait ArbPlace {
  import ArbAngle._
  import ArbDeclination._

  implicit val arbPlace: Arbitrary[Place] =
    Arbitrary {
      for {
        lat <- arbitrary[Declination]
        lon <- arbitrary[Angle]
        alt <- arbitrary[Double]
      } yield Place(lat, lon, alt)
    }

  implicit val cogCoordinates: Cogen[Place] =
    Cogen[(Declination, Angle, Double)].contramap(loc =>
      (loc.latitude, loc.longitude, loc.altitude)
    )
}

object ArbPlace extends ArbPlace

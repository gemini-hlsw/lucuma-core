// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.arb

import gsp.math.{ Angle, Lat, Lon, Place }
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import java.time.ZoneId

trait ArbPlace {
  import ArbAngle._
  import ArbDeclination._
  import ArbTime._

  private val MinAltitude: Double = 0.0
  private val MaxAltitude: Double = 8000.0

  val genEarthAlt: Gen[Double] =
    Gen.chooseNum(MinAltitude, MaxAltitude)

  implicit val arbPlace: Arbitrary[Place] =
    Arbitrary {
      for {
        lat    <- arbitrary[Lat]
        lon    <- arbitrary[Lon]
        alt    <- genEarthAlt
        zoneId <- arbitrary[ZoneId]
      } yield Place(lat, lon, alt, zoneId)
    }

  implicit val cogCoordinates: Cogen[Place] =
    Cogen[(Lat, Angle, Double)].contramap(loc => (loc.latitude, loc.longitude, loc.altitude))
}

object ArbPlace extends ArbPlace

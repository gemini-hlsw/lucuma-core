// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import coulomb.*
import coulomb.syntax.*
import coulomb.units.si.Meter
import eu.timepit.refined.*
import eu.timepit.refined.numeric.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.arb.ArbTime
import lucuma.core.math.Angle
import lucuma.core.math.Lat
import lucuma.core.math.Lon
import lucuma.core.math.Place
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen.*
import org.scalacheck.*

import java.time.ZoneId

trait ArbPlace {
  import ArbAngle.*
  import ArbDeclination.*
  import ArbTime.*

  private val MinAltitude: Int = 0
  private val MaxAltitude: Int = 8000

  val genEarthAlt: Gen[Quantity[NonNegInt, Meter]] =
    Gen
      .chooseNum(MinAltitude, MaxAltitude)
      .map { a =>
        refineV[NonNegative](a)
          .getOrElse(sys.error("Min altitude is non negative"))
      }
      .map(_.withUnit[Meter])

  val genPlace: Gen[Place] =
    for {
      lat    <- arbitrary[Lat]
      lon    <- arbitrary[Lon]
      alt    <- genEarthAlt
      zoneId <- arbitrary[ZoneId]
    } yield Place(lat, lon, alt, zoneId)

  implicit val arbPlace: Arbitrary[Place] =
    Arbitrary(genPlace)

  implicit val cogCoordinates: Cogen[Place] =
    Cogen[(Lat, Angle, BigDecimal)].contramap(loc =>
      (loc.latitude, loc.longitude, loc.altitude.value.value)
    )
}

object ArbPlace extends ArbPlace

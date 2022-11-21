// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import coulomb._
import coulomb.si.Meter
import eu.timepit.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.arb.ArbTime
import lucuma.core.math.Angle
import lucuma.core.math.Lat
import lucuma.core.math.Lon
import lucuma.core.math.Place
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import org.scalacheck._

import java.time.ZoneId

trait ArbPlace {
  import ArbAngle._
  import ArbDeclination._
  import ArbTime._

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

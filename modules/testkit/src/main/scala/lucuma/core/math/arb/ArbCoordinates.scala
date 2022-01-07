// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import org.scalacheck._

trait ArbCoordinates {
  import ArbAngle._
  import ArbRightAscension._
  import ArbDeclination._

  implicit val arbCoordinates: Arbitrary[Coordinates] =
    Arbitrary {
      for {
        ra  <- arbitrary[RightAscension]
        dec <- arbitrary[Declination]
      } yield Coordinates(ra, dec)
    }

  implicit val cogCoordinates: Cogen[Coordinates] =
    Cogen[(RightAscension, Declination)].contramap(cs => (cs.ra, cs.dec))

  // Strings that are often parsable as Coordinates
  val strings: Gen[String] =
    for {
      hms <- stringsHMS
      dms <- stringsDMS
      n   <- Gen.choose(1,5)
    } yield hms + (" " * n) + dms

}

object ArbCoordinates extends ArbCoordinates

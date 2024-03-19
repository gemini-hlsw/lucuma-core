// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import org.scalacheck.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen.*

trait ArbCoordinates {
  import ArbAngle.*
  import ArbRightAscension.given
  import ArbDeclination.given

  given Arbitrary[Coordinates] =
    Arbitrary {
      for {
        ra  <- arbitrary[RightAscension]
        dec <- arbitrary[Declination]
      } yield Coordinates(ra, dec)
    }

  given Cogen[Coordinates] =
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

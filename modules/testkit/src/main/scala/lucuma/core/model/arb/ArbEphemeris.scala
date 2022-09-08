// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import cats.kernel.Order.catsKernelOrderingForOrder
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbCoordinates
import lucuma.core.math.arb.ArbOffset
import lucuma.core.model.Ephemeris
import lucuma.core.model.EphemerisCoordinates
import lucuma.core.util.Timestamp
import lucuma.core.util.arb.ArbTimestamp
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck._

trait ArbEphemeris {
  import ArbCoordinates._
  import ArbOffset._
  import ArbTimestamp._
  import Ephemeris.Element

  implicit val arbEphemerisCoordinates: Arbitrary[EphemerisCoordinates] =
    Arbitrary {
      for {
        c <- arbitrary[Coordinates]
        o <- arbitrary[Offset]
      } yield EphemerisCoordinates(c, o)
    }

  implicit val arbElement: Arbitrary[Element] =
    Arbitrary {
      for {
        t <- arbitrary[Timestamp]
        c <- arbitrary[EphemerisCoordinates]
      } yield (t, c)
    }

  implicit val arbEphemeris: Arbitrary[Ephemeris] =
    Arbitrary {
      for {
        len <- choose(0, 10)
        es  <- listOfN(len, arbitrary[Element])
      } yield Ephemeris(es: _*)
    }

  implicit val cogEphemerisCoordinates: Cogen[EphemerisCoordinates] =
    Cogen[(Coordinates, Offset)].contramap(co => (co.coord, co.delta))

  implicit val cogEphemeris: Cogen[Ephemeris] =
    Cogen[Map[Timestamp, EphemerisCoordinates]].contramap(_.toMap)

}

object ArbEphemeris extends ArbEphemeris

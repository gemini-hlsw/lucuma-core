// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import cats.kernel.Order.catsKernelOrderingForOrder
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbCoordinates
import lucuma.core.math.arb.ArbOffset
import lucuma.core.util.Timestamp
import lucuma.core.util.arb.ArbTimestamp
import org.scalacheck.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.Gen.*

trait ArbEphemeris {
  import ArbCoordinates.given
  import ArbOffset.given
  import ArbTimestamp.given
  import Ephemeris.Element

  given Arbitrary[EphemerisCoordinates] =
    Arbitrary {
      for {
        c <- arbitrary[Coordinates]
        o <- arbitrary[Offset]
      } yield EphemerisCoordinates(c, o)
    }

  given Arbitrary[Element] =
    Arbitrary {
      for {
        t <- arbitrary[Timestamp]
        c <- arbitrary[EphemerisCoordinates]
      } yield (t, c)
    }

  given Arbitrary[Ephemeris] =
    Arbitrary {
      for {
        len <- choose(0, 10)
        es  <- listOfN(len, arbitrary[Element])
      } yield Ephemeris(es*)
    }

  given Cogen[EphemerisCoordinates] =
    Cogen[(Coordinates, Offset)].contramap(co => (co.coord, co.delta))

  given Cogen[Ephemeris] =
    Cogen[Map[Timestamp, EphemerisCoordinates]].contramap(_.toMap)

}

object ArbEphemeris extends ArbEphemeris

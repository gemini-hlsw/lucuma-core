// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import cats.implicits._
import lucuma.core.enum.MagnitudeBand
import lucuma.core.enum.MagnitudeSystem
import lucuma.core.math.MagnitudeValue
import lucuma.core.math.arb.ArbMagnitudeValue._
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import org.scalacheck._

import scala.collection.immutable.SortedMap

trait ArbMagnitude {

  import ArbEnumerated._

  implicit val arbMagnitude: Arbitrary[Magnitude] =
    Arbitrary {
      for {
        v <- arbitrary[MagnitudeValue]
        b <- arbitrary[MagnitudeBand]
        e <- arbitrary[Option[MagnitudeValue]]
        s <- arbitrary[MagnitudeSystem]
      } yield Magnitude(v, b, e, s)
    }

  implicit val cogMagnitude: Cogen[Magnitude] =
    Cogen[(MagnitudeValue, MagnitudeBand, Option[MagnitudeValue], MagnitudeSystem)].contramap { u =>
      (u.value, u.band, u.error, u.system)
    }

  implicit val arbMagnitudesMap: Arbitrary[SortedMap[MagnitudeBand, Magnitude]] =
    Arbitrary(arbitrary[Vector[Magnitude]].map(_.fproductLeft(_.band)).map(x => SortedMap(x: _*)))

  implicit val cogMagnitudesMap: Cogen[SortedMap[MagnitudeBand, Magnitude]] =
    Cogen[Vector[(MagnitudeBand, Magnitude)]].contramap(_.toVector)
}

object ArbMagnitude extends ArbMagnitude

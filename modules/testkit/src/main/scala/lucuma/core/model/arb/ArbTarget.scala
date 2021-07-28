// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enum.MagnitudeBand
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen._
import org.scalacheck._

import scala.collection.immutable.SortedMap

trait ArbTarget {

  import ArbEphemerisKey._
  import ArbSiderealTracking._
  import ArbMagnitude._
  import ArbEnumerated._

  implicit val arbTarget: Arbitrary[Target] =
    Arbitrary {
      for {
        n <- arbitrary[NonEmptyString]
        t <- arbitrary[Either[EphemerisKey, SiderealTracking]]
        m <- arbitrary[Vector[(MagnitudeBand, Magnitude)]]
      } yield Target(n, t, SortedMap(m: _*))
    }

  implicit val cogTarget: Cogen[Target] =
    Cogen[(String, Either[EphemerisKey, SiderealTracking], Vector[(MagnitudeBand, Magnitude)])]
      .contramap { t =>
        (t.name.value, t.track, t.magnitudes.toVector)
      }
}

object ArbTarget extends ArbTarget

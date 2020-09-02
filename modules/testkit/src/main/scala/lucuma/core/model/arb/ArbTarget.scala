// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import scala.collection.immutable.SortedMap

import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enum.MagnitudeBand
import lucuma.core.math.ProperMotion
import lucuma.core.math.arb._
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen._
import org.scalacheck._

trait ArbTarget {

  import ArbEphemerisKey._
  import ArbProperMotion._
  import ArbMagnitude._
  import ArbEnumerated._

  implicit val arbTarget: Arbitrary[Target] =
    Arbitrary {
      for {
        n <- arbitrary[NonEmptyString]
        i <- arbitrary[Option[NonEmptyString]]
        t <- arbitrary[Either[EphemerisKey, ProperMotion]]
        m <- arbitrary[Vector[(MagnitudeBand, Magnitude)]]
      } yield Target(n, i, t, SortedMap(m: _*))
    }

  implicit val cogTarget: Cogen[Target] =
    Cogen[
      (
        String,
        Option[String],
        Either[EphemerisKey, ProperMotion],
        Vector[(MagnitudeBand, Magnitude)]
      )
    ].contramap { t =>
      (t.name.value, t.catalogId.map(_.value), t.track, t.magnitudes.toVector)
    }
}

object ArbTarget extends ArbTarget

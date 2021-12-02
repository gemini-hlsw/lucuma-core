// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import cats.syntax.all._
import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enum.Band
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen._
import org.scalacheck._

import scala.collection.immutable.SortedMap

trait ArbTarget {

  import ArbEphemerisKey._
  import ArbSiderealTracking._
  import ArbTargetBrightness._
  import ArbAngularSize._
  import ArbEnumerated._

  implicit val arbSiderealTarget: Arbitrary[SiderealTarget] =
    Arbitrary {
      for {
        n <- arbitrary[NonEmptyString]
        t <- arbitrary[SiderealTracking]
        m <- arbitrary[Vector[(Band, TargetBrightness)]]
        s <- arbitrary[Option[AngularSize]]
      } yield SiderealTarget(n, t, SortedMap(m: _*), s)
    }

  implicit val arbNonsiderealTarget: Arbitrary[NonsiderealTarget] =
    Arbitrary {
      for {
        n <- arbitrary[NonEmptyString]
        t <- arbitrary[EphemerisKey]
        m <- arbitrary[Vector[(Band, TargetBrightness)]]
        s <- arbitrary[Option[AngularSize]]
      } yield NonsiderealTarget(n, t, SortedMap(m: _*), s)
    }

  implicit val arbTarget: Arbitrary[Target] = Arbitrary(
    Gen.oneOf(arbitrary[SiderealTarget], arbitrary[NonsiderealTarget])
  )

  implicit val cogSiderealTarget: Cogen[SiderealTarget] =
    Cogen[(String, SiderealTracking, Vector[(Band, TargetBrightness)])]
      .contramap(t => (t.name.value, t.tracking, t.brightnesses.toVector))

  implicit val cogNonsiderealTarget: Cogen[NonsiderealTarget] =
    Cogen[(String, EphemerisKey, Vector[(Band, TargetBrightness)])]
      .contramap(t => (t.name.value, t.ephemerisKey, t.brightnesses.toVector))

  implicit val cogTarget: Cogen[Target] =
    Cogen[Either[SiderealTarget, NonsiderealTarget]]
      .contramap {
        case t @ SiderealTarget(_, _, _, _)    => t.asLeft
        case t @ NonsiderealTarget(_, _, _, _) => t.asRight
      }
}

object ArbTarget extends ArbTarget

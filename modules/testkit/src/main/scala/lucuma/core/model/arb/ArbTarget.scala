// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import cats.syntax.all.*
import eu.timepit.refined.scalacheck.string.*
import eu.timepit.refined.types.string.NonEmptyString
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

trait ArbTarget {

  import ArbEphemerisKey.given
  import ArbSiderealTracking.given
  import ArbSourceProfile.given
  import ArbCatalogInfo.given

  given Arbitrary[Target.Sidereal] =
    Arbitrary {
      for {
        n <- arbitrary[NonEmptyString]
        t <- arbitrary[SiderealTracking]
        b <- arbitrary[SourceProfile]
        c <- arbitrary[Option[CatalogInfo]]
      } yield Target.Sidereal(n, t, b, c)
    }

  given Arbitrary[Target.Nonsidereal] =
    Arbitrary {
      for {
        n <- arbitrary[NonEmptyString]
        t <- arbitrary[EphemerisKey]
        b <- arbitrary[SourceProfile]
      } yield Target.Nonsidereal(n, t, b)
    }

  given Arbitrary[Target] = Arbitrary(
    Gen.oneOf(arbitrary[Target.Sidereal], arbitrary[Target.Nonsidereal])
  )

  given Cogen[Target.Sidereal] =
    Cogen[(String, SiderealTracking, SourceProfile, Option[CatalogInfo])]
      .contramap(t => (t.name.value, t.tracking, t.sourceProfile, t.catalogInfo))

  given Cogen[Target.Nonsidereal] =
    Cogen[(String, EphemerisKey, SourceProfile)]
      .contramap(t => (t.name.value, t.ephemerisKey, t.sourceProfile))

  given Cogen[Target] =
    Cogen[Either[Target.Sidereal, Target.Nonsidereal]]
      .contramap {
        case t @ Target.Sidereal(_, _, _, _) => t.asLeft
        case t @ Target.Nonsidereal(_, _, _) => t.asRight
      }
}

object ArbTarget extends ArbTarget

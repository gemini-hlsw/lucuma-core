// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import cats.syntax.all._
import eu.timepit.refined.scalacheck.string._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen._
import org.scalacheck._

trait ArbTarget {

  import ArbEphemerisKey._
  import ArbSiderealTracking._
  import ArbEnumerated._
  import ArbSourceProfile._
  import ArbCatalogInfo._

  implicit val ArbSiderealTarget: Arbitrary[Target.Sidereal] =
    Arbitrary {
      for {
        n <- arbitrary[NonEmptyString]
        t <- arbitrary[SiderealTracking]
        b <- arbitrary[SourceProfile]
        c <- arbitrary[Option[CatalogInfo]]
      } yield Target.Sidereal(n, t, b, c)
    }

  implicit val arbNonsiderealTarget: Arbitrary[Target.Nonsidereal] =
    Arbitrary {
      for {
        n <- arbitrary[NonEmptyString]
        t <- arbitrary[EphemerisKey]
        b <- arbitrary[SourceProfile]
      } yield Target.Nonsidereal(n, t, b)
    }

  implicit val arbTarget: Arbitrary[Target] = Arbitrary(
    Gen.oneOf(arbitrary[Target.Sidereal], arbitrary[Target.Nonsidereal])
  )

  implicit val cogSiderealTarget: Cogen[Target.Sidereal] =
    Cogen[(String, SiderealTracking, SourceProfile, Option[CatalogInfo])]
      .contramap(t => (t.name.value, t.tracking, t.sourceProfile, t.catalogInfo))

  implicit val cogNonsiderealTarget: Cogen[Target.Nonsidereal] =
    Cogen[(String, EphemerisKey, SourceProfile)]
      .contramap(t => (t.name.value, t.ephemerisKey, t.sourceProfile))

  implicit val cogTarget: Cogen[Target] =
    Cogen[Either[Target.Sidereal, Target.Nonsidereal]]
      .contramap {
        case t @ Target.Sidereal(_, _, _, _) => t.asLeft
        case t @ Target.Nonsidereal(_, _, _) => t.asRight
      }
}

object ArbTarget extends ArbTarget

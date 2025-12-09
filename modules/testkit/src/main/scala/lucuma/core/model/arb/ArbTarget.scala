// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import eu.timepit.refined.scalacheck.string.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.math.Region
import lucuma.core.math.arb.ArbRegion
import lucuma.core.model.Target.Nonsidereal
import lucuma.core.model.Target.Opportunity
import lucuma.core.model.Target.Sidereal
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

trait ArbTarget {

  import ArbEphemerisKey.given
  import ArbSiderealTracking.given
  import ArbSourceProfile.given
  import ArbCatalogInfo.given
  import ArbRegion.given

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
        t <- arbitrary[Ephemeris.Key]
        b <- arbitrary[SourceProfile]
      } yield Target.Nonsidereal(n, t, b)
    }

  given Arbitrary[Target.Opportunity] =
    Arbitrary {
      for {
        n <- arbitrary[NonEmptyString]
        r <- arbitrary[Region]
        b <- arbitrary[SourceProfile]
      } yield Target.Opportunity(n, r, b)
    }
    
  given Arbitrary[Target] = Arbitrary(
    Gen.oneOf(
      arbitrary[Target.Sidereal], 
      arbitrary[Target.Nonsidereal],
      arbitrary[Target.Opportunity]
    )
  )

  given Cogen[Target.Sidereal] =
    Cogen[(String, SiderealTracking, SourceProfile, Option[CatalogInfo])]
      .contramap(t => (t.name.value, t.tracking, t.sourceProfile, t.catalogInfo))

  given Cogen[Target.Nonsidereal] =
    Cogen[(String, Ephemeris.Key, SourceProfile)]
      .contramap(t => (t.name.value, t.ephemerisKey, t.sourceProfile))

  given Cogen[Target.Opportunity] =
    Cogen[(String, Region, SourceProfile)]
      .contramap(t => (t.name.value, t.region, t.sourceProfile))
    
  given Cogen[Target] =
    Cogen[Target]: (s, t) =>
      t match
        case t @ Sidereal(_, _, _, _) => Cogen[Sidereal].perturb(s, t)
        case t @ Nonsidereal(_, _, _) => Cogen[Nonsidereal].perturb(s, t)
        case t @ Opportunity(_, _, _) => Cogen[Opportunity].perturb(s, t)

}

object ArbTarget extends ArbTarget

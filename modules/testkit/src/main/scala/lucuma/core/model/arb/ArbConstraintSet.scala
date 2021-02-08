// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import eu.timepit.refined.scalacheck._
import eu.timepit.refined.scalacheck.string.nonEmptyStringArbitrary
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enum._
import lucuma.core.model.ElevationRange
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import lucuma.core.model.ConstraintSet

trait ArbConstraintSet {
  import ArbElevationRange._
  import ArbEnumerated._

  implicit val arbConstraintSet: Arbitrary[ConstraintSet] =
    Arbitrary {
      for {
        name <- arbitrary[NonEmptyString]
        iq   <- arbitrary[ImageQuality]
        ce   <- arbitrary[CloudExtinction]
        sb   <- arbitrary[SkyBackground]
        wv   <- arbitrary[WaterVapor]
        er   <- arbitrary[ElevationRange]
      } yield ConstraintSet(name, iq, ce, sb, wv, er)
    }

  implicit val cogConstraintSet: Cogen[ConstraintSet] =
    Cogen[
      (NonEmptyString, ImageQuality, CloudExtinction, SkyBackground, WaterVapor, ElevationRange)
    ].contramap(cs =>
      (cs.name,
       cs.imageQuality,
       cs.cloudExtinction,
       cs.skyBackground,
       cs.waterVapor,
       cs.elevationRange
      )
    )
}

object ArbConstraintSet extends ArbConstraintSet

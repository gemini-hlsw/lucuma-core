// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package model
package arb

import coulomb._
import eu.timepit.refined.scalacheck.numeric._
import eu.timepit.refined.types.numeric._
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.math.units.DeciArcSecond
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

trait ArbImageQuality {
  import ArbEnumerated._

  implicit val arbImageQuality: Arbitrary[ImageQuality] =
    Arbitrary {
      for {
        q <- arbitrary[PosInt]
      } yield ImageQuality(q.withUnit[DeciArcSecond])
    }

  implicit val cogSemester: Cogen[ImageQuality] =
    Cogen[Int].contramap(_.toDeciArcSeconds.value.value)

}

object ArbImageQuality extends ArbImageQuality

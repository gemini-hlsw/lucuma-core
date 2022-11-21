// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import lucuma.core.math.HourAngle
import lucuma.core.math.RightAscension
import org.scalacheck.Arbitrary._
import org.scalacheck._

trait ArbRightAscension {
  import ArbAngle._

  implicit val arbRightAscension: Arbitrary[RightAscension] =
    Arbitrary(arbitrary[HourAngle].map(RightAscension.fromHourAngle.get))

  implicit val cogRightAscension: Cogen[RightAscension] =
    Cogen[HourAngle].contramap(RightAscension.fromHourAngle.reverseGet)

}

object ArbRightAscension extends ArbRightAscension

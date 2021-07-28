// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import lucuma.core.math.Angle
import lucuma.core.math.Declination
import org.scalacheck.Arbitrary._
import org.scalacheck._

trait ArbDeclination {
  import ArbAngle._

  implicit val arbDeclination: Arbitrary[Declination] =
    Arbitrary(arbitrary[Angle].map(Declination.fromAngleWithCarry(_)._1))

  implicit val cogDeclination: Cogen[Declination] =
    Cogen[Angle].contramap(_.toAngle)

}

object ArbDeclination extends ArbDeclination

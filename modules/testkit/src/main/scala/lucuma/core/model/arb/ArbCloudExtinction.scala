// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package model
package arb

import eu.timepit.refined.scalacheck.numeric._
import eu.timepit.refined.types.numeric._
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

trait ArbCloudExtinction {
  import ArbEnumerated._

  implicit val arbCloudExtinction: Arbitrary[CloudExtinction] =
    Arbitrary {
      for {
        mg <- arbitrary[NonNegInt]
      } yield CloudExtinction(mg)
    }

  implicit val cogSemester: Cogen[CloudExtinction] =
    Cogen[Int].contramap(_.toDeciMagnitudes.value)

}

object ArbCloudExtinction extends ArbCloudExtinction

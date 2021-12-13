// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import lucuma.core.math.Angle
import lucuma.core.math.arb.ArbAngle
import org.scalacheck.Arbitrary._
import org.scalacheck._

trait ArbGaussianSource {
  import ArbAngle._

  implicit val arbGaussianSource: Arbitrary[GaussianSource] =
    Arbitrary {
      for {
        a <- arbitrary[Angle]
      } yield GaussianSource(a)
    }

  implicit val cogGaussianSource: Cogen[GaussianSource] =
    Cogen[Angle].contramap(_.fwhm)
}

object ArbGaussianSource extends ArbGaussianSource

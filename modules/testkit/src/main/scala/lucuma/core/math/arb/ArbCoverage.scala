// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
package arb

import cats.syntax.option._
import cats.syntax.order._

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen._
import org.scalacheck._

trait ArbCoverage {
  import ArbWavelength._

  implicit val arbCoverage: Arbitrary[Coverage] =
    Arbitrary {
      for {
        w0 <- arbitrary[Wavelength]
        w1 <- arbitrary[Wavelength]
      } yield if (w0 < w1) Coverage(w0, w1) else Coverage(w1, w0)
    }

  implicit val cogCoverage: Cogen[Coverage] =
    Cogen[Option[(Wavelength, Wavelength)]].contramap {
      case Coverage.Empty           => none
      case Coverage.Range(min, max) => (min, max).some
    }

}

object ArbCoverage extends ArbCoverage

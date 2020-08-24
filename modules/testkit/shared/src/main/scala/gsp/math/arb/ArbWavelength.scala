// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.arb

import eu.timepit.refined.scalacheck.numeric._
import eu.timepit.refined.types.numeric.PosInt
import gsp.math.Wavelength
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen._
import org.scalacheck._

trait ArbWavelength {

  implicit val arbWavelength: Arbitrary[Wavelength] = Arbitrary {
    arbitrary[PosInt].map(Wavelength(_))
  }

  implicit val cogWavelength: Cogen[Wavelength] =
    Cogen[Int].contramap(_.toPicometers.value.value)

}

object ArbWavelength extends ArbWavelength

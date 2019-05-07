// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.arb

import gsp.math.Wavelength
import gsp.math.syntax.prism._
import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Cogen._

trait ArbWavelength {

  implicit val arbWavelength: Arbitrary[Wavelength] =
    Arbitrary(choose(0, Int.MaxValue).map(Wavelength.fromPicometers.unsafeGet(_)))

  implicit val cogWavelength: Cogen[Wavelength] =
    Cogen[Int].contramap(_.toPicometers)

}

object ArbWavelength extends ArbWavelength

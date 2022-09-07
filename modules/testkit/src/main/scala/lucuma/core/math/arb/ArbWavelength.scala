// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
package arb

import eu.timepit.refined.scalacheck.numeric._
import eu.timepit.refined.types.numeric.PosInt
import lucuma.refined._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen._
import org.scalacheck._

trait ArbWavelength {
  val RedWavelength: Wavelength = Wavelength.picometers.get(620000.refined)

  implicit val arbWavelength: Arbitrary[Wavelength] = Arbitrary(
    Gen.frequency(
      1  -> RedWavelength,
      20 -> arbitrary[PosInt].map(Wavelength(_))
    )
  )

  implicit val cogWavelength: Cogen[Wavelength] =
    Cogen[Int].contramap(_.toPicometers.value.value)

  private[this] val intBoundedBigDecimals: Gen[BigDecimal] =
    for {
      s  <- Gen.choose(0, 10)
      bd <- arbitrary[Int].map(BigDecimal(_))
    } yield BigDecimal(bd.underlying.movePointLeft(s))

  val bigDecimalWavelengths: Gen[BigDecimal] =
    Gen.oneOf(intBoundedBigDecimals, arbitrary[BigDecimal])

}

object ArbWavelength extends ArbWavelength

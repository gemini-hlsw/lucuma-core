// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
package arb

import eu.timepit.refined.scalacheck.numeric.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.refined.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen.*

trait ArbWavelength {
  val RedWavelength: Wavelength = Wavelength.picometers.get(620000.refined)

  given Arbitrary[Wavelength] = Arbitrary(
    Gen.frequency(
      1  -> RedWavelength,
      20 -> arbitrary[PosInt].map(Wavelength(_))
    )
  )

  given Cogen[Wavelength] =
    Cogen[Int].contramap(_.toPicometers.value.value)

  private val intBoundedBigDecimals: Gen[BigDecimal] =
    for {
      s  <- Gen.choose(0, 10)
      bd <- arbitrary[Int].map(BigDecimal(_))
    } yield BigDecimal(bd.underlying.movePointLeft(s))

  val bigDecimalWavelengths: Gen[BigDecimal] =
    Gen.oneOf(intBoundedBigDecimals, arbitrary[BigDecimal])

}

object ArbWavelength extends ArbWavelength

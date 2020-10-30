// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import lucuma.core.arb._
import lucuma.core.math.MagnitudeValue
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import org.scalacheck._

trait ArbMagnitudeValue {

  implicit val arbMagnitudeValue: Arbitrary[MagnitudeValue] = Arbitrary {
    arbitrary[Int].map(MagnitudeValue.apply)
  }

  implicit val cogMagnitudeValue: Cogen[MagnitudeValue] =
    Cogen[Int].contramap(_.scaledValue)

  // Strings that are often parseable as a magnitude value
  val stringsMagnitudeValue: Gen[String] =
    arbitrary[MagnitudeValue]
      .map(_.toDoubleValue.toString)
      .flatMapOneOf(
        Gen.const,
        _ => arbitrary[BigDecimal].toString,
        _ => arbitrary[String]  // swap for a random String
      )

}

object ArbMagnitudeValue extends ArbMagnitudeValue

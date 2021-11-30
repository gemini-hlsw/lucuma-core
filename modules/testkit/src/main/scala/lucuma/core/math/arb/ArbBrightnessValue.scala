// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import lucuma.core.arb._
import lucuma.core.math.BrightnessValue
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import org.scalacheck._

trait ArbBrightnessValue {

  implicit val arbBrightnessValue: Arbitrary[BrightnessValue] = Arbitrary {
    arbitrary[Int].map(BrightnessValue.apply)
  }

  implicit val cogBrightnessValue: Cogen[BrightnessValue] =
    Cogen[Int].contramap(_.scaledValue)

  // Strings that are often parseable as a magnitude value
  val stringsBrightnessValue: Gen[String] =
    arbitrary[BrightnessValue]
      .map(_.toDoubleValue.toString)
      .flatMapOneOf(
        Gen.const,
        _ => arbitrary[BigDecimal].toString,
        _ => arbitrary[String] // swap for a random String
      )

}

object ArbBrightnessValue extends ArbBrightnessValue

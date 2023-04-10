// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
package arb

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

trait ArbSignalToNoise {

  given Arbitrary[SignalToNoise] =
    Arbitrary {
      validBigDecimalSignalToNoise.map(SignalToNoise.unsafeFromBigDecimalExact)
    }

  given Cogen[SignalToNoise] =
    Cogen[BigDecimal].contramap(_.toBigDecimal)

  private val validBigDecimalSignalToNoise: Gen[BigDecimal] =
    Gen
      .choose(1L, 9_999_999_999L)
      .map(BigDecimal(_, 3))

  private val coverageExamples: Gen[BigDecimal] =
    Gen
      .choose(1L, 9_999_999_999_999L)
      .map(BigDecimal(_, 6))

  val bigDecimalSignalToNoise: Gen[BigDecimal] =
    Gen.oneOf(validBigDecimalSignalToNoise, coverageExamples, arbitrary[BigDecimal])

  val stringSignalToNoise: Gen[String] = {
    val v = validBigDecimalSignalToNoise.map(_.toString)
    Gen.oneOf(
      v,
      v.map(s => s"${s}0"),
      arbitrary[String]
    )
  }

}

object ArbSignalToNoise extends ArbSignalToNoise

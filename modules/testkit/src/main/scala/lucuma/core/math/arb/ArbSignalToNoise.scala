// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
package arb

import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.refineV
import eu.timepit.refined.types.numeric.NonNegBigDecimal
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbSignalToNoise {

  given Arbitrary[SignalToNoise] =
    Arbitrary {
      validBigDecimalSignalToNoise.map(SignalToNoise.unsafeFromBigDecimalExact)
    }

  given Cogen[SignalToNoise] =
    Cogen[BigDecimal].contramap(_.toBigDecimal)

  private val validBigDecimalSignalToNoise: Gen[BigDecimal] =
    Gen
      .choose(0L, 9_999_999_999L)
      .map(BigDecimal(_, 3))

  private val validNonNegBigDecimalSignalToNoise: Gen[NonNegBigDecimal] =
    validBigDecimalSignalToNoise.map(refineV[NonNegative](_).getOrElse(throw new RuntimeException))

  private val coverageExamples: Gen[BigDecimal] =
    Gen
      .choose(0L, 9_999_999_999_999L)
      .map(BigDecimal(_, 6))

  private val coverageExamplesNonNegBigDecimal: Gen[NonNegBigDecimal] =
    coverageExamples.map(refineV[NonNegative](_).getOrElse(throw new RuntimeException))

  val bigDecimalSignalToNoise: Gen[BigDecimal] =
    Gen.oneOf(validBigDecimalSignalToNoise, coverageExamples, arbitrary[BigDecimal])

  val nonNegBigDecimalSignalToNoise: Gen[NonNegBigDecimal] =
    Gen.oneOf(validNonNegBigDecimalSignalToNoise, coverageExamplesNonNegBigDecimal, validNonNegBigDecimalSignalToNoise)

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

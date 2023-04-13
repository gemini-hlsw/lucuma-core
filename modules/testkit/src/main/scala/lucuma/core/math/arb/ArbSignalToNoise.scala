// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math
package arb

import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.math.arb.ArbRefined.given
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.*

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

  private val validPosBigDecimalSignalToNoise: Gen[PosBigDecimal] =
    validBigDecimalSignalToNoise.map(refineV[Positive](_).getOrElse(throw new RuntimeException))

  private val coverageExamples: Gen[BigDecimal] =
    Gen
      .choose(1L, 9_999_999_999_999L)
      .map(BigDecimal(_, 6))

  private val coverageExamplesPosBigDecimal: Gen[PosBigDecimal] =
    coverageExamples.map(refineV[Positive](_).getOrElse(throw new RuntimeException))

  val bigDecimalSignalToNoise: Gen[BigDecimal] =
    Gen.oneOf(validBigDecimalSignalToNoise, coverageExamples, arbitrary[BigDecimal])

  val posBigDecimalSignalToNoise: Gen[PosBigDecimal] =
    Gen.oneOf(validPosBigDecimalSignalToNoise, coverageExamplesPosBigDecimal, arbitrary[PosBigDecimal])

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

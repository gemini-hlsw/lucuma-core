// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.kernel.laws.discipline.OrderTests
import eu.timepit.refined.cats.*
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.refineV
import eu.timepit.refined.types.numeric.NonNegBigDecimal
import io.circe.testing.CodecTests
import io.circe.testing.instances.*
import lucuma.core.math.arb.ArbSignalToNoise
import lucuma.core.optics.laws.discipline.FormatTests
import lucuma.core.optics.laws.discipline.ValidSplitEpiTests
import monocle.law.discipline.*
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

final class SignalToNoiseSuite extends munit.DisciplineSuite {

  import ArbSignalToNoise.given
  import ArbSignalToNoise.bigDecimalSignalToNoise
  import ArbSignalToNoise.nonNegBigDecimalSignalToNoise
  import ArbSignalToNoise.stringSignalToNoise

  given Arbitrary[NonNegBigDecimal] = Arbitrary {
    Gen.posNum[BigDecimal].map(refineV[NonNegative](_).getOrElse(throw new RuntimeException))
  }

  checkAll("Order",                     OrderTests[SignalToNoise].order)
  checkAll("JSON Codec",                CodecTests[SignalToNoise].codec)
  checkAll("FromBigDecimalExact",       PrismTests(SignalToNoise.FromBigDecimalExact))
  checkAll("FromBigDecimalRounding",
    ValidSplitEpiTests(SignalToNoise.FromBigDecimalRounding).validSplitEpiWith(bigDecimalSignalToNoise))
  checkAll("FromNonNegBigDecimalExact", PrismTests(SignalToNoise.FromNonNegBigDecimalExact))
  checkAll("FromNonNegBigDecimalRounding",
    ValidSplitEpiTests(SignalToNoise.FromNonNegBigDecimalRounding).validSplitEpiWith(nonNegBigDecimalSignalToNoise)
  )
  checkAll("FromString",                FormatTests(SignalToNoise.FromString).formatWith(stringSignalToNoise))

}

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Order
import cats.kernel.laws.discipline.OrderTests
import io.circe.testing.CodecTests
import io.circe.testing.instances.*
import lucuma.core.math.arb.ArbSignalToNoise
import lucuma.core.optics.Format
import lucuma.core.optics.laws.discipline.FormatTests
import monocle.law.discipline.*
import org.scalacheck.Prop.*


final class SignalToNoiseSuite extends munit.DisciplineSuite {

  import ArbSignalToNoise.given
  import ArbSignalToNoise.bigDecimalSignalToNoise
  import ArbSignalToNoise.stringSignalToNoise

  checkAll("Order",                  OrderTests[SignalToNoise].order)
  checkAll("JSON Codec",             CodecTests[SignalToNoise].codec)
  checkAll("FromBigDecimalExact",    PrismTests(SignalToNoise.FromBigDecimalExact))
  checkAll("FromBigDecimalRounding", FormatTests(SignalToNoise.FromBigDecimalRounding).formatWith(bigDecimalSignalToNoise))
  checkAll("FromString",             FormatTests(SignalToNoise.FromString).formatWith(stringSignalToNoise))

}

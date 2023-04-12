// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Order
import cats.kernel.laws.discipline.OrderTests
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import io.circe.testing.CodecTests
import io.circe.testing.instances.*
import lucuma.core.math.arb.ArbRefined.given
import lucuma.core.math.arb.ArbSignalToNoise
import lucuma.core.optics.Format
import lucuma.core.optics.laws.discipline.FormatTests
import monocle.law.discipline.*
import org.scalacheck.Prop.*
import org.typelevel.discipline.Predicate
import spire.laws.*
import spire.syntax.all.*


final class SignalToNoiseSuite extends munit.DisciplineSuite {

  import ArbSignalToNoise.given
  import ArbSignalToNoise.bigDecimalSignalToNoise
  import ArbSignalToNoise.posBigDecimalSignalToNoise
  import ArbSignalToNoise.stringSignalToNoise

  given Predicate[SignalToNoise] = Predicate.const(true)

  checkAll("Order",                     OrderTests[SignalToNoise].order)
  checkAll("JSON Codec",                CodecTests[SignalToNoise].codec)
  checkAll("FromBigDecimalExact",       PrismTests(SignalToNoise.FromBigDecimalExact))
  checkAll("FromBigDecimalRounding",    FormatTests(SignalToNoise.FromBigDecimalRounding).formatWith(bigDecimalSignalToNoise))
  checkAll("FromPosBigDecimalExact",    PrismTests(SignalToNoise.FromPosBigDecimalExact))
  checkAll("FromPosBigDecimalRounding", FormatTests(SignalToNoise.FromPosBigDecimalRounding).formatWith(posBigDecimalSignalToNoise))
  checkAll("FromString",                FormatTests(SignalToNoise.FromString).formatWith(stringSignalToNoise))
  checkAll("Additive",                  RingLaws[SignalToNoise].additiveSemigroup)

  test("operations") {
    assertEquals(SignalToNoise.Min + SignalToNoise.Min, SignalToNoise.unsafeFromBigDecimalExact(0.002))
  }
}

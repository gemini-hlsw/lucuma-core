// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import cats.Eval
import lucuma.core.arb.ArbEval
import lucuma.core.arb.ArbTime
import lucuma.core.math.skycalc.solver.Samples
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

import java.time.Instant
import scala.collection.immutable.TreeMap

trait ArbSamples {
  import ArbEval.given
  import ArbTime.given

  def genSamples[A: Arbitrary]: Gen[Samples[A]] =
    arbitrary[TreeMap[Instant, Eval[A]]].map(Samples.fromMap)

  given arbSamples[A: Arbitrary]: Arbitrary[Samples[A]] =
    Arbitrary(genSamples[A])

  given cogenSamples[A: Cogen]: Cogen[Samples[A]] =
    Cogen[Map[Instant, A]].contramap(_.toMap.view.mapValues(_.value).toMap)
}

object ArbSamples extends ArbSamples

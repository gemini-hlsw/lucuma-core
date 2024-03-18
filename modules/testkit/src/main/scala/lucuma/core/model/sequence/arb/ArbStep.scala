// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import lucuma.core.enums.Breakpoint
import lucuma.core.enums.ObserveClass
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbUid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbStep {
  import ArbEnumerated.given
  import ArbStepEstimate.given
  import ArbStepConfig.given
  import ArbUid.given

  given [D: Arbitrary]: Arbitrary[Step[D]] =
    Arbitrary {
      for {
        i <- arbitrary[Step.Id]
        d <- arbitrary[D]
        c <- arbitrary[StepConfig]
        e <- arbitrary[StepEstimate]
        o <- arbitrary[ObserveClass]
        b <- arbitrary[Breakpoint]
      } yield Step(i, d, c, e, o, b)
    }

  given [D: Cogen]: Cogen[Step[D]] =
    Cogen[(
      Step.Id,
      D,
      StepConfig,
      StepEstimate,
      Breakpoint
    )].contramap { a =>
      (a.id,
       a.instrumentConfig,
       a.stepConfig,
       a.estimate,
       a.breakpoint
      )
    }

}

object ArbStep extends ArbStep

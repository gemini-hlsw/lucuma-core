// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbObservationWorkflow:
  import ArbEnumerated.given
  import ArbObservationValidation.given

  given Arbitrary[ObservationWorkflow] =
    Arbitrary(
      for {
        s <- arbitrary[ObservationWorkflowState]
        t <- arbitrary[List[ObservationWorkflowState]]
        v <- arbitrary[List[ObservationValidation]]
      } yield ObservationWorkflow(s, t, v)
    )

  given Cogen[ObservationWorkflow] =
    Cogen[
      (
        ObservationWorkflowState,
        List[ObservationWorkflowState],
        List[ObservationValidation]
      )
    ]
  .contramap(w => (w.state, w.validTransitions, w.validationErrors))

object ArbObservationWorkflow extends ArbObservationWorkflow

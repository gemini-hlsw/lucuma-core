// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.derived.*
import io.circe.Codec
import lucuma.core.enums.ObservationWorkflowState
import monocle.Focus
import monocle.Lens

final case class ObservationWorkflow(
  state: ObservationWorkflowState,
  validTransitions: List[ObservationWorkflowState],
  validationErrors: List[ObservationValidation]
) derives Codec, Eq


object ObservationWorkflow:
  val state: Lens[ObservationWorkflow, ObservationWorkflowState] = 
    Focus[ObservationWorkflow](_.state)
  val validTransitions: Lens[ObservationWorkflow, List[ObservationWorkflowState]] =
    Focus[ObservationWorkflow](_.validTransitions)
  val validationErrors: Lens[ObservationWorkflow, List[ObservationValidation]] =
    Focus[ObservationWorkflow](_.validationErrors)


// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.derived.*
import io.circe.Codec
import lucuma.core.enums.ObservationWorkflowState

final case class ObservationWorkflow(
  state: ObservationWorkflowState,
  validTransitions: List[ObservationWorkflowState],
  validationErrors: List[ObservationValidation]
) derives Codec, Eq


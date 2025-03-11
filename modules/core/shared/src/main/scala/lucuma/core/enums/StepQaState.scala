// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for dataset QA state.
 * @group Enumerations
 */
enum StepQaState(val tag: String) derives Enumerated:
  case Pass extends StepQaState("Pass")
  case Fail extends StepQaState("Fail")

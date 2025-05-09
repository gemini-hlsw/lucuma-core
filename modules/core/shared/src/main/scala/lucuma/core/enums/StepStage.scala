// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum StepStage(val tag: String, val name: String, val description: String) derives Enumerated {

  case Abort          extends StepStage("abort",           "Abort Step",      "A step was aborted abruptly.")
  case Continue       extends StepStage("continue",        "Continue Step",   "A paused step was continued.")
  case EndConfigure   extends StepStage("end_configure",   "End Configure",   "End of instrument configuration stage.")
  case EndObserve     extends StepStage("end_observe",     "End Observe",     "End of data collection for all datasets produced by this step.")
  case EndStep        extends StepStage("end_step",        "End Step",        "Step complete.")
  case Pause          extends StepStage("pause",           "Pause Step",      "A step was paused mid-execution.")
  case StartConfigure extends StepStage("start_configure", "Start Configure", "Start of instrument configuration stage.")
  case StartObserve   extends StepStage("start_observe",   "Start Observe",   "Start of data collection for this step.")
  case StartStep      extends StepStage("start_step",      "Start Step",      "Step started.")
  case Stop           extends StepStage("stop",            "Stop Step",       "A step was stopped gracefully before completion.")

}
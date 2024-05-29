// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum SequenceCommand(
  val tag: String,
  val name: String,
  val isTerminal: Boolean,
  val description: String
) derives Enumerated {

  case Abort    extends SequenceCommand("abort",    "Abort",    isTerminal = true,  "Sequence was aborted without finishing any ongoing dataset(s).")
  case Continue extends SequenceCommand("continue", "Continue", isTerminal = false, "Sequence was continued after having been paused.")
  case Pause    extends SequenceCommand("pause",    "Pause",    isTerminal = false, "Sequence was temporarily paused.")
  case Start    extends SequenceCommand("start",    "Start",    isTerminal = false, "Sequence execution started.")
  case Stop     extends SequenceCommand("stop",     "Stop",     isTerminal = true,  "Sequence execution was stopped, after first finishing any currently executing atom.")

}

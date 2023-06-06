// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum SequenceCommand(val tag: String, val name: String, val description: String) derives Enumerated {

  case Abort    extends SequenceCommand("abort",    "Abort",    "Sequence was aborted without finishing any ongoing dataset(s).")
  case Continue extends SequenceCommand("continue", "Continue", "Sequence was continued after having been paused.")
  case Pause    extends SequenceCommand("pause",    "Pause",    "Sequence was temporarily paused.")
  case Start    extends SequenceCommand("start",    "Start",    "Sequence execution started.")
  case Stop     extends SequenceCommand("stop",     "Stop",     "Sequence execution was stopped, after first finishing any currently executing atom.")

}

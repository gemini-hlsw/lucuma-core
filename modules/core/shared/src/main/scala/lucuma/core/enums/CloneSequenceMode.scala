// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * What a cloned observation takes from its source's materialized (stored)
 * acquisition and science sequences.  Each sequence type is handled on its own,
 * and one with nothing to copy is generated as usual.
 */
enum CloneSequenceMode(val tag: String, val name: String) derives Enumerated, Display:

  /** Copy nothing; the clone generates its sequences from its own configuration. */
  case None         extends CloneSequenceMode("none",          "Generate a new sequence")

  /** Copy every step, whatever state it ended in, as a pending step. */
  case AllSteps     extends CloneSequenceMode("all_steps",     "Copy all steps")

  /** Copy only the steps that have not started executing. */
  case PendingSteps extends CloneSequenceMode("pending_steps", "Copy pending steps")

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum AtomStage(val tag: String, val name: String, val description: String) derives Enumerated:
  case EndAtom   extends AtomStage("end_atom",   "End Atom",   "Atom complete.")
  case StartAtom extends AtomStage("start_atom", "Start Atom", "Atom started.")

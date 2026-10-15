// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * The kind of setup performed during a visit.  A full setup sets up on the
 * target from scratch, typically starting with a slew.  A reacquisition
 * recenters on the target after science has started, without slewing.
 */
enum SetupKind(val tag: String) derives Enumerated:
  case Full          extends SetupKind("full")
  case Reacquisition extends SetupKind("reacquisition")

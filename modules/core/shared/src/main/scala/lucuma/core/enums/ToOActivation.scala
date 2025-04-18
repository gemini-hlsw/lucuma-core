// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum ToOActivation(val tag: String, val label: String) derives Enumerated:
  case None     extends ToOActivation("none", "None")
  case Standard extends ToOActivation("standard", "Standard")
  case Rapid    extends ToOActivation("rapid", "Rapid")

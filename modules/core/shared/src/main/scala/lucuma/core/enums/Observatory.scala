// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum Observatory(val tag: String) derives Enumerated:
  case Gemini extends Observatory("gemini")
  case Keck   extends Observatory("keck")
  case Subaru extends Observatory("subaru")

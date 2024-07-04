// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum ScienceBand(val tag: String) derives Enumerated:
  case Band1 extends ScienceBand("band1")
  case Band2 extends ScienceBand("band2")
  case Band3 extends ScienceBand("band3")
  case Band4 extends ScienceBand("band4")
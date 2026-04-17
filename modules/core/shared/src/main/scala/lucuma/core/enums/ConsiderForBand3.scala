// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum ConsiderForBand3(val tag: String) derives Enumerated:
  case Unset         extends ConsiderForBand3("unset")
  case Consider      extends ConsiderForBand3("consider")
  case DoNotConsider extends ConsiderForBand3("do_not_consider")

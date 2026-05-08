// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.util.Enumerated

enum GmosFpuType(val tag: String) derives Enumerated:
  case LongSlit extends GmosFpuType("LongSlit")
  case Ns       extends GmosFpuType("Ns")
  case Ifu      extends GmosFpuType("Ifu")

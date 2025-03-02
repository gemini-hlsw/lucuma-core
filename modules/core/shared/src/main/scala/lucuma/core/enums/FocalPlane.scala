// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum FocalPlane(val tag: String) derives Enumerated:
  case SingleSlit   extends FocalPlane("single_slit")
  case MultipleSlit extends FocalPlane("multiple_slit")
  case IFU          extends FocalPlane("ifu")

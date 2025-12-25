// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.util.Enumerated

enum GmosImagingVariantType(val tag: String, val display: String) derives Enumerated:
  case Grouped     extends GmosImagingVariantType("grouped", "Grouped")
  case Interleaved extends GmosImagingVariantType("interleaved", "Interleaved")
  case PreImaging  extends GmosImagingVariantType("pre_imaging", "Pre-Imaging")
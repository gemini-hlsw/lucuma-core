// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum CatalogName(val tag: String) derives Enumerated:
  case Simbad extends CatalogName("simbad")
  case Gaia extends CatalogName("gaia")
  case Import extends CatalogName("import")

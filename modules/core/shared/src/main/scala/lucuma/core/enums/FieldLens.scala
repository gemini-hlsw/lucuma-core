// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum FieldLens(val tag: String) derives Enumerated:
  case In extends FieldLens("in")
  case Out extends FieldLens("out")


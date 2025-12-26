// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.util.Enumerated

enum TelescopeConfigGeneratorType(val tag: String) derives Enumerated:
  case NoGenerator extends TelescopeConfigGeneratorType("none")
  case Enumerated  extends TelescopeConfigGeneratorType("enumerated")
  case Random      extends TelescopeConfigGeneratorType("random")
  case Spiral      extends TelescopeConfigGeneratorType("spiral")
  case Uniform     extends TelescopeConfigGeneratorType("uniform")
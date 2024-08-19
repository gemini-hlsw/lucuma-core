// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

enum ProgramUserRole(val tag: String) derives Enumerated:

  case Pi      extends ProgramUserRole("pi")
  case Coi     extends ProgramUserRole("coi")
  case CoiRO   extends ProgramUserRole("coi_ro")
  case Support extends ProgramUserRole("support")

end ProgramUserRole
// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Indicates a user's Educational Status.
 */
enum EducationalStatus(val tag: String) derives Enumerated:
  case PhD              extends EducationalStatus("phd")
  case GradStudent      extends EducationalStatus("grad_student")
  case UndergradStudent extends EducationalStatus("undergrad_student")
  case Other            extends EducationalStatus("other")

end EducationalStatus

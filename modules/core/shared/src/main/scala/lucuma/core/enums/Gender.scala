// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Indicates a user's reported gender.
 */
enum Gender(val tag: String) derives Enumerated:
  case Male         extends Gender("male")
  case Female       extends Gender("female")
  case Other        extends Gender("other")
  case NotSpecified extends Gender("not_specified")

end Gender

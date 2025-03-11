// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for user target type.
 * @group Enumerations (Generated)
 */
enum UserTargetType(
  val tag: String,
  val shortName: String,
  val longName: String,
  val obsolete: Boolean
) derives Enumerated:
  case BlindOffset extends UserTargetType("BlindOffset", "Blind Offset", "Blind Offset", false)
  case OffAxis extends UserTargetType("OffAxis", "Off Axis", "Off Axis", false)
  case TuningStar extends UserTargetType("TuningStar", "Tuning Star", "Tuning Star", false)
  case Other extends UserTargetType("Other", "Other", "Other", false)

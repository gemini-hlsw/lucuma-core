// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.refined

import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.boolean.Not
import eu.timepit.refined.generic.Equal
import shapeless.nat._0

object numeric {
  type NonZeroInt  = Int Refined Not[Equal[_0]]
  object NonZeroInt extends RefinedTypeOps.Numeric[NonZeroInt, Int]
}



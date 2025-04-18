// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.refined

import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedTypeOps
import eu.timepit.refined.boolean.Not
import eu.timepit.refined.generic.Equal

object numeric {
  type NonZero = Not[Equal[0]]

  type NonZeroInt  = Int Refined NonZero
  object NonZeroInt extends RefinedTypeOps.Numeric[NonZeroInt, Int]

  type NonZeroBigDecimal = BigDecimal Refined NonZero
  object NonZeroBigDecimal extends RefinedTypeOps[NonZeroBigDecimal, BigDecimal]
}



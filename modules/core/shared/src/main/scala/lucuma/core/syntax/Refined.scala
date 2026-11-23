// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.syntax

import eu.timepit.refined.types.numeric.NonNegInt

import scala.annotation.targetName

trait ToRefinedOps:

  extension (self: NonNegInt)

    /**
     * Adds two NonNegInt values, capping the result at `Int.MaxValue` rather
     * than overflowing.
     */
    def saturatingAdd(other: NonNegInt): NonNegInt =
      NonNegInt.unsafeFrom((self.value.toLong + other.value).min(Int.MaxValue).toInt)

    /**
     * Alias for `saturatingAdd`, matching the bounded `+|` of `Timestamp` and
     * `TimeSpan`.
     */
    @targetName("saturatingAddOp")
    def +|(other: NonNegInt): NonNegInt =
      saturatingAdd(other)

object refined extends ToRefinedOps

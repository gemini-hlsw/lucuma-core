// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.syntax

import eu.timepit.refined.types.all.NonNegInt
import eu.timepit.refined.types.all.NonNegLong
import lucuma.core.model.NonNegDuration


trait ToNonNegDurationOps {
  extension(self: NonNegDuration)

    def toNanos: NonNegLong =
      NonNegLong.unsafeFrom(self.value.toNanos)

    def +(that: NonNegDuration): NonNegDuration =
      NonNegDuration.unsafeFrom(self.value.plus(that.value))

    def *(that: NonNegInt): NonNegDuration =
      NonNegDuration.unsafeFrom(self.value.multipliedBy(that.value.toLong))

}

object nonnegduration extends ToNonNegDurationOps

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.syntax

import eu.timepit.refined.types.all.{ NonNegInt, NonNegLong }
import lucuma.core.model.NonNegDuration

final class NonNegDurationOps(self: NonNegDuration) {

  def toNanos: NonNegLong =
    NonNegLong.unsafeFrom(self.value.toNanos)

  def +(that: NonNegDuration): NonNegDuration =
    NonNegDuration.unsafeFrom(self.value.plus(that.value))

  def *(that: NonNegInt): NonNegDuration =
    NonNegDuration.unsafeFrom(self.value.multipliedBy(that.value.toLong))

}

trait ToNonNegDurationOps {
  implicit def toNonNegDurationOps(s: NonNegDuration): NonNegDurationOps =
    new NonNegDurationOps(s)
}

object nonnegduration extends ToNonNegDurationOps

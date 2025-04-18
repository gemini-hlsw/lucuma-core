// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package syntax

import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval

trait ToNightOps {

  extension(self: Night) {

    def toTimestampInterval: Option[TimestampInterval] = {
      val bi = self.interval
      for {
        s <- Timestamp.fromInstantTruncated(bi.lower)
        e <- Timestamp.fromInstantTruncated(bi.upper)
      } yield TimestampInterval.between(s, e)
    }

  }

}

object night extends ToNightOps

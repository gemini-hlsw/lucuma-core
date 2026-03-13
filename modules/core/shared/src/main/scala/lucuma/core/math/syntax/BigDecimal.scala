// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.syntax

import lucuma.core.math.Offset

trait toBigDecimalOps:
  extension(self: BigDecimal)
    def qArcsec: Offset.Q =
      Offset.Q.signedDecimalArcseconds.reverseGet(self)

    def pArcsec: Offset.P =
      Offset.P.signedDecimalArcseconds.reverseGet(self)

object bigDecimal extends toBigDecimalOps

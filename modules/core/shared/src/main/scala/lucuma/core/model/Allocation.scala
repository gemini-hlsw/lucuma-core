// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Order
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.util.TimeSpan

final case class Allocation(
  category: TimeAccountingCategory,
  scienceBand: ScienceBand,
  duration: TimeSpan
):

  override def toString =
    f"Allocation($category, $scienceBand, ${duration.toHours}%3.2f h)"

object Allocation:

  given Order[Allocation] =
    Order.by(a => (a.category, a.scienceBand, a.duration))
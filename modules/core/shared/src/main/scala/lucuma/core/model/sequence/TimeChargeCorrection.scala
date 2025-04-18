// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Order
import lucuma.core.enums.ChargeClass
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp

case class TimeChargeCorrection(
  timestamp:   Timestamp,
  user:        User,
  chargeClass: ChargeClass,
  op:          TimeChargeCorrection.Op,
  amount:      TimeSpan,
  comment:     Option[String]
)

object TimeChargeCorrection {

  enum Op(val tag: String, val name: String, val description: String) derives Enumerated {
    case Add      extends Op("add", "Add", "Adds a time span to the time accounting category for an observation")
    case Subtract extends Op("subtract", "Subtract", "Subtracts a time span from the time accounting category for an observation")
  }

  given Order[TimeChargeCorrection] =
    Order.by { a => (
      a.timestamp,
      a.user.displayName,
      a.user.id,
      a.op,
      a.amount,
      a.comment
    )}

}

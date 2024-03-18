// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import lucuma.core.enums.ChargeClass
import lucuma.core.model.User
import lucuma.core.model.arb.ArbUser
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbTimeSpan
import lucuma.core.util.arb.ArbTimestamp
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbTimeChargeCorrection {

  import TimeChargeCorrection.Op

  import ArbEnumerated.given
  import ArbTimestamp.given
  import ArbTimeSpan.given
  import ArbUser.given

  given Arbitrary[TimeChargeCorrection] =
    Arbitrary {
      for {
        t <- arbitrary[Timestamp]
        u <- arbitrary[User]
        c <- arbitrary[ChargeClass]
        o <- arbitrary[Op]
        s <- arbitrary[TimeSpan]
        m <- arbitrary[Option[String]]
      } yield TimeChargeCorrection(t, u, c, o, s, m)
    }

  given Cogen[TimeChargeCorrection] =
    Cogen[(
      Timestamp,
      User,
      ChargeClass,
      Op,
      TimeSpan,
      Option[String]
    )].contramap { a => (
      a.timestamp,
      a.user,
      a.chargeClass,
      a.op,
      a.amount,
      a.comment
    )}

}

object ArbTimeChargeCorrection extends ArbTimeChargeCorrection

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import eu.timepit.refined.scalacheck.string.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.util.Timestamp
import lucuma.core.util.arb.ArbGid
import lucuma.core.util.arb.ArbTimestamp
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbAttribution:
  import ArbGid.given
  import ArbTimestamp.given

  given Arbitrary[Attribution] =
    Arbitrary:
      for
        u <- arbitrary[User.Id]
        t <- arbitrary[Timestamp]
        r <- arbitrary[Option[NonEmptyString]]
      yield Attribution(u, t, r)

  given Cogen[Attribution] =
    Cogen[(User.Id, Timestamp, Option[String])].contramap(a => (a.by, a.at, a.reason.map(_.value)))

object ArbAttribution extends ArbAttribution

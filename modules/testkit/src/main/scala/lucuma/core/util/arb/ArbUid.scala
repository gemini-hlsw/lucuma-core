// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util.arb

import lucuma.core.util.Uid
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

import java.util.UUID

trait ArbUid {
  given arbUid[A: Uid]: Arbitrary[A] =
    Arbitrary(arbitrary[UUID].map(Uid[A].isoUuid.reverseGet))

  given cogUid[A: Uid]: Cogen[A] =
    Cogen[UUID].contramap(Uid[A].isoUuid.get(_))
}

object ArbUid extends ArbUid

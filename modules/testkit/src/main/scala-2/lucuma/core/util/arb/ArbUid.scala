// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util.arb

import lucuma.core.util.Uid
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

import java.util.UUID

trait ArbUid {
  implicit def arbUid[A: Uid]: Arbitrary[A] =
    Arbitrary(arbitrary[UUID].map(Uid[A].isoUuid.reverseGet))

  implicit def cogUid[A: Uid]: Cogen[A] =
    Cogen[UUID].contramap(Uid[A].isoUuid.get(_))
}

object ArbUid extends ArbUid

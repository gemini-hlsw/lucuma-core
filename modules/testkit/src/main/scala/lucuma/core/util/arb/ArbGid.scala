// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util.arb

import eu.timepit.refined.scalacheck.numeric.*
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.util.Gid
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbGid {
  given arbGid[A](using ev: Gid[A]): Arbitrary[A] =
    Arbitrary(arbitrary[PosLong].map(ev.isoPosLong.reverseGet))

  given cogGid[A](using ev: Gid[A]): Cogen[A] =
    Cogen[Long].contramap(ev.isoPosLong.get(_).value)
}

object ArbGid extends ArbGid

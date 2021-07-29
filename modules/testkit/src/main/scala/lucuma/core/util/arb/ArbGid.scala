// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util.arb

import eu.timepit.refined.scalacheck.numeric._
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.util.Gid
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck._

trait ArbGid {

  implicit def arbGid[A](implicit ev: Gid[A]): Arbitrary[A] =
    Arbitrary(arbitrary[PosLong].map(ev.isoPosLong.reverseGet))

  implicit def cogGid[A](implicit ev: Gid[A]): Cogen[A] =
    Cogen[Long].contramap(ev.isoPosLong.get(_).value)

}

object ArbGid extends ArbGid

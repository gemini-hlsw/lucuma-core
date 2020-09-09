// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util.arb

import lucuma.core.util.Gid
import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.scalacheck.numeric._

trait ArbGid {

  implicit def ArbGid[A](implicit ev: Gid[A]): Arbitrary[A] =
    Arbitrary(arbitrary[PosLong].map(ev.isoPosLong.reverseGet))

  implicit def CogGif[A](implicit ev: Gid[A]): Cogen[A] =
    Cogen[Long].contramap(ev.isoPosLong.get(_).value)

}

object ArbGid extends ArbGid

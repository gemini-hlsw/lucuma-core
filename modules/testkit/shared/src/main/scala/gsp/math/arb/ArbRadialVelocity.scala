// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.arb

import gsp.math._
import gsp.math.syntax.prism._
import gsp.math.PhysicalConstants.SpeedOfLight
import org.scalacheck._
import org.scalacheck.Cogen

trait ArbRadialVelocity {

  implicit val arbRadialVelocity: Arbitrary[RadialVelocity] =
    Arbitrary {
      for {
        v <- Gen.choose(-SpeedOfLight + 1, SpeedOfLight - 1)
      } yield RadialVelocity.fromMetersPerSecond.unsafeGet(v)
    }

  implicit val cogRadialVelocity: Cogen[RadialVelocity] =
    Cogen[Int].contramap(_.toMetersPerSecond.value)

}

object ArbRadialVelocity extends ArbRadialVelocity

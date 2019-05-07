// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.arb

import gsp.math._
import gsp.math.syntax.prism._
import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen

trait ArbRadialVelocity {

  implicit val arbRadialVelocity: Arbitrary[RadialVelocity] =
    Arbitrary(arbitrary[Short].map(n => RadialVelocity.fromMetersPerSecond.unsafeGet(n.toInt)))

  implicit val cogRadialVelocity: Cogen[RadialVelocity] =
    Cogen[Int].contramap(_.toMetersPerSecond)

}

object ArbRadialVelocity extends ArbRadialVelocity

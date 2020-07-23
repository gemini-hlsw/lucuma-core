// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.arb

import org.scalacheck._
import org.scalacheck.Gen._
import gsp.math.skycalc.TwilightBoundType

trait ArbTwilightBoundType {
  val genTwilightBoundType: Gen[TwilightBoundType] =
    oneOf(TwilightBoundType.all)

  implicit val arbTwilightBoundType: Arbitrary[TwilightBoundType] =
    Arbitrary(genTwilightBoundType)

  implicit val cogTwilightBoundType: Cogen[TwilightBoundType] =
    Cogen[String].contramap(_.name)
}

object ArbTwilightBoundType extends ArbTwilightBoundType

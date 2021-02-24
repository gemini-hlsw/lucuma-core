// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package model
package arb

import lucuma.core.enum.TwilightType
import lucuma.core.model.{ ObservingNight, TwilightBoundedNight }
import lucuma.core.model.arb.ArbObservingNight._
import lucuma.core.util.arb.ArbEnumerated._

import org.scalacheck._
import org.scalacheck.Arbitrary._

trait ArbTwilightBoundedNight {

  implicit val arbTwilightBoundedNight: Arbitrary[TwilightBoundedNight] =
    Arbitrary {
      (
        for {
          b <- arbitrary[TwilightType]
          n <- arbitrary[ObservingNight]
        } yield n.twilightBounded(b)
      ).suchThat(_.isDefined).map(_.get)
    }

  implicit val cogTwilightBoundedNight: Cogen[TwilightBoundedNight] =
    Cogen[(TwilightType, ObservingNight)].contramap(o => (o.twilightType, o.toObservingNight))
}

object ArbTwilightBoundedNight extends ArbTwilightBoundedNight

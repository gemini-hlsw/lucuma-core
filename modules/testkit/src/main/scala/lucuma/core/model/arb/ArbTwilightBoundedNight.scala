// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package model
package arb

import lucuma.core.enums.TwilightType
import lucuma.core.model.arb.ArbObservingNight.*
import lucuma.core.util.arb.ArbEnumerated.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.*

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

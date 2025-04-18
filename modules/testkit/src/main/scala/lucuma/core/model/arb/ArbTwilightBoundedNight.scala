// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package model
package arb

import lucuma.core.enums.TwilightType
import lucuma.core.model.arb.ArbObservingNight.given
import lucuma.core.util.arb.ArbEnumerated.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

trait ArbTwilightBoundedNight {

  given Arbitrary[TwilightBoundedNight] =
    Arbitrary {
      (
        for {
          b <- arbitrary[TwilightType]
          n <- arbitrary[ObservingNight]
        } yield n.twilightBounded(b)
      ).suchThat(_.isDefined).map(_.get)
    }

  given Cogen[TwilightBoundedNight] =
    Cogen[(TwilightType, ObservingNight)].contramap(o => (o.twilightType, o.toObservingNight))
}

object ArbTwilightBoundedNight extends ArbTwilightBoundedNight

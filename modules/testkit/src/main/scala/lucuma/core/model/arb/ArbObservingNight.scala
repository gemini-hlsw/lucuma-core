// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package model
package arb

import lucuma.core.arb.ArbTime
import lucuma.core.enums.Site
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

import java.time.LocalDate

trait ArbObservingNight {
  import ArbEnumerated.given
  import ArbTime.given

  given Arbitrary[LocalObservingNight] =
    Arbitrary {
      arbitrary[LocalDate].map(LocalObservingNight(_))
    }

  given Arbitrary[ObservingNight] =
    Arbitrary {
      for {
        n <- arbitrary[LocalObservingNight]
        s <- arbitrary[Site]
      } yield n.atSite(s)
    }

  given Cogen[LocalObservingNight] =
    Cogen[LocalDate].contramap(_.toLocalDate)

  given Cogen[ObservingNight] =
    Cogen[(Site, LocalObservingNight)].contramap(o => (o.site, o.toLocalObservingNight))
}

object ArbObservingNight extends ArbObservingNight

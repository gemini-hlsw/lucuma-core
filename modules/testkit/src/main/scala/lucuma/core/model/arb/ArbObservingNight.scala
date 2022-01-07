// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package model
package arb

import lucuma.core.arb.ArbTime
import lucuma.core.enum.Site
import lucuma.core.model.LocalObservingNight
import lucuma.core.model.ObservingNight
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary._
import org.scalacheck._

import java.time.LocalDate

trait ArbObservingNight {
  import ArbEnumerated._
  import ArbTime._

  implicit val arbLocalObservingNight: Arbitrary[LocalObservingNight] =
    Arbitrary {
      arbitrary[LocalDate].map(LocalObservingNight(_))
    }

  implicit val arbObservingNight: Arbitrary[ObservingNight] =
    Arbitrary {
      for {
        n <- arbitrary[LocalObservingNight]
        s <- arbitrary[Site]
      } yield n.atSite(s)
    }

  implicit val cogLocalObservingNight: Cogen[LocalObservingNight] =
    Cogen[LocalDate].contramap(_.toLocalDate)

  implicit val cogObservingNight: Cogen[ObservingNight] =
    Cogen[(Site, LocalObservingNight)].contramap(o => (o.site, o.toLocalObservingNight))
}

object ArbObservingNight extends ArbObservingNight

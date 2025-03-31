// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.enums.ComaOption
import lucuma.core.enums.TipTiltSource
import lucuma.core.model.M2GuideConfig
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbNewType.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbM2GuideConfig {

  given arbM2GuideOn: Arbitrary[M2GuideConfig.M2GuideOn] =
    Arbitrary {
      for {
        c <- arbitrary[ComaOption]
        s <- arbitrary[List[TipTiltSource]]
      } yield M2GuideConfig.M2GuideOn(c, s.sortBy(x => s"$x").toSet)
    }

  given m2GuideOnCogen: Cogen[M2GuideConfig.M2GuideOn] =
    Cogen[(Boolean, List[TipTiltSource])].contramap(x =>
      (x.coma.value, x.sources.toList)
    )

  given arbM2GuideConfig: Arbitrary[M2GuideConfig] =
    Arbitrary {
      for {
        off <- Gen.const(M2GuideConfig.M2GuideOff)
        on  <- arbitrary[M2GuideConfig.M2GuideOn]
        l   <- Gen.oneOf(off, on)
      } yield l
    }

  given m2GuideConfigCogen: Cogen[M2GuideConfig] =
    Cogen[Option[M2GuideConfig.M2GuideOn]].contramap {
      case x: M2GuideConfig.M2GuideOn => Some(x)
      case _                          => None
    }
}

object ArbM2GuideConfig extends ArbM2GuideConfig

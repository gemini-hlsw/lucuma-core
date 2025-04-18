// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import lucuma.core.enums.M1Source
import lucuma.core.model.M1GuideConfig
import lucuma.core.util.arb.ArbEnumerated.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbM1GuideConfig {

  given arbM1GuideOn: Arbitrary[M1GuideConfig.M1GuideOn] =
    Arbitrary {
      for {
        s <- arbitrary[M1Source]
      } yield M1GuideConfig.M1GuideOn(s)
    }

  given m1GuideOnCogen: Cogen[M1GuideConfig.M1GuideOn] =
    Cogen[M1Source].contramap(_.source)

  given arbM1GuideConfig: Arbitrary[M1GuideConfig] =
    Arbitrary {
      for {
        off <- Gen.const(M1GuideConfig.M1GuideOff)
        on  <- arbitrary[M1GuideConfig.M1GuideOn]
        l   <- Gen.oneOf(off, on)
      } yield l
    }

  given m1GuideConfigCogen: Cogen[M1GuideConfig] =
    Cogen[Option[M1GuideConfig.M1GuideOn]].contramap {
      case x: M1GuideConfig.M1GuideOn => Some(x)
      case _                          => None
    }
}

object ArbM1GuideConfig extends ArbM1GuideConfig

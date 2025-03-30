// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import cats.syntax.all.*
import lucuma.core.enums.*
import lucuma.core.model.GemsConfig
import lucuma.core.model.GemsConfig.GemsOff
import lucuma.core.model.GemsConfig.GemsOn
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Cogen.*
import org.scalacheck.Gen
import lucuma.core.util.arb.ArbNewType.given

trait ArbGemsConfig {

  given Arbitrary[GemsOn] = Arbitrary {
    for {
      c1  <- arbitrary[Cwfs1Usage]
      c2  <- arbitrary[Cwfs2Usage]
      c3  <- arbitrary[Cwfs3Usage]
      od1 <- arbitrary[Odgw1Usage]
      od2 <- arbitrary[Odgw2Usage]
      od3 <- arbitrary[Odgw3Usage]
      od4 <- arbitrary[Odgw4Usage]
      oi  <- arbitrary[OIUsage]
      p1  <- arbitrary[P1Usage]
    } yield GemsOn(c1, c2, c3, od1, od2, od3, od4, p1, oi)
  }

  given Cogen[GemsOn] =
    Cogen[(Boolean, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean)]
      .contramap(x =>
        (
          x.cwfs1 === Cwfs1Usage.Use,
          x.cwfs2 === Cwfs2Usage.Use,
          x.cwfs3 === Cwfs3Usage.Use,
          x.odgw1 === Odgw1Usage.Use,
          x.odgw2 === Odgw2Usage.Use,
          x.odgw3 === Odgw3Usage.Use,
          x.odgw4 === Odgw4Usage.Use,
          x.useP1 === P1Usage.Use,
          x.useOI === OIUsage.Use
        )
      )

  given Arbitrary[GemsConfig] = Arbitrary {
    Gen.oneOf(arbitrary[GemsOn], Gen.const(GemsOff))
  }

  given Cogen[GemsConfig] = Cogen[Option[GemsOn]].contramap {
    case GemsOff   => None
    case x: GemsOn => Some(x)
  }

}

object ArbGemsConfig extends ArbGemsConfig

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.arb

import coulomb.Quantity
import coulomb.testkit.given
import coulomb.units.accepted.Millimeter
import lucuma.core.model.AltairConfig
import lucuma.core.model.AltairConfig.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbAltairConfig {

  given Arbitrary[Lgs] = Arbitrary {
    for {
      st <- arbitrary[Boolean]
      sf <- arbitrary[Boolean]
      l1 <- arbitrary[Quantity[BigDecimal, Millimeter]]
      l2 <- arbitrary[Quantity[BigDecimal, Millimeter]]
    } yield Lgs(st, sf, (l1, l2))
  }

  given Cogen[Lgs] =
    Cogen[(
      Boolean,
      Boolean,
      (Quantity[BigDecimal, Millimeter], Quantity[BigDecimal, Millimeter])
    )]
      .contramap(x => (x.strap, x.sfo, x.starPos))

  given Arbitrary[Ngs] = Arbitrary {
    for {
      b  <- arbitrary[Boolean]
      l1 <- arbitrary[Quantity[BigDecimal, Millimeter]]
      l2 <- arbitrary[Quantity[BigDecimal, Millimeter]]
    } yield Ngs(b, (l1, l2))
  }

  given Cogen[Ngs] =
    Cogen[(
      Boolean,
      (Quantity[BigDecimal, Millimeter], Quantity[BigDecimal, Millimeter])
    )].contramap(x => (x.blend, x.starPos))

  given Arbitrary[AltairConfig] = Arbitrary {
    Gen.oneOf(arbitrary[Lgs],
              arbitrary[Ngs],
              Gen.const(AltairOff),
              Gen.const(LgsWithP1),
              Gen.const(LgsWithOi)
    )
  }

  given Cogen[AltairConfig] = Cogen[Option[
    Option[Option[Either[(Boolean,
                          Boolean,
                          Quantity[BigDecimal, Millimeter],
                          Quantity[BigDecimal, Millimeter]
                         ),
                         (Boolean, Quantity[BigDecimal, Millimeter], Quantity[BigDecimal, Millimeter])
    ]]]
  ]].contramap {
    case AltairOff    => None
    case LgsWithP1    => Some(None)
    case LgsWithOi    => Some(Some(None))
    case Lgs(a, b, c) => Some(Some(Some(Left(a, b, c._1, c._2))))
    case Ngs(a, c)    => Some(Some(Some(Right(a, c._1, c._2))))
  }

}

object ArbAltairConfig extends ArbAltairConfig

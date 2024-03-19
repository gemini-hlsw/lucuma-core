// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import org.scalacheck.Cogen
import spire.math.*

trait CogenSpire {
  implicit val safeLongCogen: Cogen[SafeLong] =
    Cogen[BigInt].contramap(_.toBigInt)

  implicit val rationalCogen: Cogen[Rational] =
    Cogen[(SafeLong, SafeLong)].contramap(r => (r.numerator, r.denominator))

  implicit val naturalCogen: Cogen[Natural] =
    Cogen[BigInt].contramap(_.toBigInt)

  implicit val numberCogen: Cogen[Number] =
    Cogen((s, n) =>
      if (n.isExact)
        Cogen[Rational].contramap[Number](_.toRational).perturb(s, n)
      else
        Cogen[BigDecimal].contramap[Number](_.toBigDecimal).perturb(s, n)
    )
}

object CogenSpire extends CogenSpire

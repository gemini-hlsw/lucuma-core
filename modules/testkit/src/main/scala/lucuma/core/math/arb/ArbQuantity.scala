// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import coulomb.*
import coulomb.syntax.*
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen

object ArbQuantity {
  given [V, U](using arbV: Arbitrary[V]): Arbitrary[Quantity[V, U]] =
    Arbitrary(arbV.arbitrary.map(_.withUnit[U]))

  given [V, U](using cogenV: Cogen[V]): Cogen[Quantity[V, U]] =
    cogenV.contramap(_.value)
}

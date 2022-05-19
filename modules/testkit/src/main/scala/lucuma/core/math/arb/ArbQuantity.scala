// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import coulomb.qopaque.{Quantity, withUnit}
import org.scalacheck.Arbitrary

object ArbQuantity {
  given [V, U](using arbV: Arbitrary[V]): Arbitrary[Quantity[V, U]] =
    Arbitrary(arbV.arbitrary.map(_.withUnit[U]))
}

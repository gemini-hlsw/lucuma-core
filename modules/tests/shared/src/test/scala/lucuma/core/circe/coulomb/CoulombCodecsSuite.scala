// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.circe.coulomb

import coulomb.Quantity
import coulomb.integrations.cats.all.given
import coulomb.units.si.Meter
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.math.arb.ArbQuantity.given
import munit.DisciplineSuite
import org.scalacheck.Arbitrary

class CoulombCodecsSuite extends DisciplineSuite with ArbitraryInstances {
  type Length = Quantity[Int, Meter]

  checkAll("LengthCodec", CodecTests[Length].codec)
}

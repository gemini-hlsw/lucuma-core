// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.util.arb._

import munit._
import cats.kernel.laws.discipline.EqTests
import io.circe.testing.CodecTests
import io.circe.testing.instances.arbitraryJson

final class OrcidIdSuite extends DisciplineSuite {
  import ArbOrcidId._

  // Laws
  checkAll("OrcidId", EqTests[OrcidId].eqv)
  checkAll("OrcidId", CodecTests[OrcidId].unserializableCodec)

}

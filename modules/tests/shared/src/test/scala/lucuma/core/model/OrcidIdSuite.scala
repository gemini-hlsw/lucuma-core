// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.implicits.*
import cats.kernel.laws.discipline.EqTests
import io.circe.testing.CodecTests
import io.circe.testing.instances.arbitraryJson
import lucuma.core.model.arb.*
import munit.*
import org.scalacheck.Prop

final class OrcidIdSuite extends DisciplineSuite {
  import ArbOrcidId.given

  // Laws
  checkAll("OrcidId", EqTests[OrcidId].eqv)
  checkAll("OrcidId", CodecTests[OrcidId].unserializableCodec)

  test("round-rip through uri") {
    Prop.forAll { (o: OrcidId) =>
      OrcidId.fromUri(o.uri) === Right(o)
    }
  }

  test("round-rip through value") {
    Prop.forAll { (o: OrcidId) =>
      OrcidId.fromValue(o.value) === Right(o)
    }
  }

}

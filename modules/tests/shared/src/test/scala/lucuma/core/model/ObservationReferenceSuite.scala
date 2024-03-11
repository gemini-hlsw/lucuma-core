// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline.*
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.arb.ArbObservationReference
import lucuma.core.optics.laws.discipline.*

final class ObservationReferenceSuite extends munit.DisciplineSuite with ArbitraryInstances {

  import ArbObservationReference.given
  import ArbObservationReference.observationReferenceStrings

  checkAll("ObservationReference", OrderTests[ObservationReference].order)
  checkAll("ObservationReference", FormatTests(ObservationReference.fromString).formatWith(observationReferenceStrings))
  checkAll("ObservationReference", CodecTests[ObservationReference].codec)
}


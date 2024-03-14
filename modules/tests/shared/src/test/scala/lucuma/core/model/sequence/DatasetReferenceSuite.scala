// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.sequence.arb.ArbDatasetReference
import lucuma.core.optics.laws.discipline.*

final class DatasetReferenceSuite extends munit.DisciplineSuite with ArbitraryInstances {

  import ArbDatasetReference.given
  import ArbDatasetReference.datasetReferenceStrings

  checkAll("DatasetReference", OrderTests[DatasetReference].order)
  checkAll("DatasetReference", FormatTests(DatasetReference.fromString).formatWith(datasetReferenceStrings))
  checkAll("DatasetReference", CodecTests[DatasetReference].codec)

}
// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import io.circe.testing.KeyCodecTests
import lucuma.core.model.sequence.Dataset
import lucuma.core.util.arb.*
import lucuma.core.util.laws.GidTests
import munit.DisciplineSuite

final class IdsSuite extends DisciplineSuite {
  import ArbGid.given

  checkAll("Gid[CallForProposals.Id]", GidTests[CallForProposals.Id].gid)
  checkAll("Gid[Dataset.Id]", GidTests[Dataset.Id].gid)
  checkAll("Gid[ExecutionEvent.Id]", GidTests[ExecutionEvent.Id].gid)
  checkAll("Gid[Observation.Id]", GidTests[Observation.Id].gid)
  checkAll("Gid[Program.Id]", GidTests[Program.Id].gid)
  checkAll("Gid[Visit.Id]", GidTests[Visit.Id].gid)

  checkAll("KeyCodec[CallForProposals.Id]", KeyCodecTests[CallForProposals.Id].keyCodec)
  checkAll("KeyCodec[Dataset.Id]", KeyCodecTests[Dataset.Id].keyCodec)
  checkAll("KeyCodec[ExecutionEvent.Id]", KeyCodecTests[ExecutionEvent.Id].keyCodec)
  checkAll("KeyCodec[Observation.Id]", KeyCodecTests[Observation.Id].keyCodec)
  checkAll("KeyCodec[Program.Id]", KeyCodecTests[Program.Id].keyCodec)
  checkAll("KeyCodec[Visit.Id]", KeyCodecTests[Visit.Id].keyCodec)
}

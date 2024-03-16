// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import lucuma.core.enums.DatasetStage
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.StepStage
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.util.Timestamp
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbGid
import lucuma.core.util.arb.ArbTimestamp
import lucuma.core.util.arb.ArbUid
import org.scalacheck.Arbitrary.*
import org.scalacheck.*

trait ArbExecutionEvent {

  import ArbEnumerated.given
  import ArbGid.*
  import ArbTimestamp.*
  import ArbUid.*

  given Arbitrary[ExecutionEvent.DatasetEvent] =
    Arbitrary {
      for {
        eid <- arbitrary[ExecutionEvent.Id]
        rec <- arbitrary[Timestamp]
        oid <- arbitrary[Observation.Id]
        vid <- arbitrary[Visit.Id]
        sid <- arbitrary[Step.Id]
        did <- arbitrary[Dataset.Id]
        stg <- arbitrary[DatasetStage]
      } yield ExecutionEvent.DatasetEvent(eid, rec, oid, vid, sid, did, stg)
    }

  given Cogen[ExecutionEvent.DatasetEvent] =
    Cogen[(
      ExecutionEvent.Id,
      Timestamp,
      Observation.Id,
      Visit.Id,
      Step.Id,
      Dataset.Id,
      DatasetStage
    )].contramap { a => (
      a.id,
      a.received,
      a.observationId,
      a.visitId,
      a.stepId,
      a.datasetId,
      a.stage
    )}

  given Arbitrary[ExecutionEvent.SequenceEvent] =
    Arbitrary {
      for {
        eid <- arbitrary[ExecutionEvent.Id]
        rec <- arbitrary[Timestamp]
        oid <- arbitrary[Observation.Id]
        vid <- arbitrary[Visit.Id]
        cmd <- arbitrary[SequenceCommand]
      } yield ExecutionEvent.SequenceEvent(eid, rec, oid, vid, cmd)
    }

  given Cogen[ExecutionEvent.SequenceEvent] =
    Cogen[(
      ExecutionEvent.Id,
      Timestamp,
      Observation.Id,
      Visit.Id,
      SequenceCommand
    )].contramap { a => (
      a.id,
      a.received,
      a.observationId,
      a.visitId,
      a.command
    )}

  given Arbitrary[ExecutionEvent.StepEvent] =
    Arbitrary {
      for {
        eid <- arbitrary[ExecutionEvent.Id]
        rec <- arbitrary[Timestamp]
        oid <- arbitrary[Observation.Id]
        vid <- arbitrary[Visit.Id]
        sid <- arbitrary[Step.Id]
        stg <- arbitrary[StepStage]
      } yield ExecutionEvent.StepEvent(eid, rec, oid, vid, sid, stg)
    }

  given Cogen[ExecutionEvent.StepEvent] =
    Cogen[(
      ExecutionEvent.Id,
      Timestamp,
      Observation.Id,
      Visit.Id,
      Step.Id,
      StepStage
    )].contramap { a => (
      a.id,
      a.received,
      a.observationId,
      a.visitId,
      a.stepId,
      a.stage
    )}

  given Arbitrary[ExecutionEvent] =
    Arbitrary {
      Gen.oneOf[ExecutionEvent](
        arbitrary[ExecutionEvent.DatasetEvent],
        arbitrary[ExecutionEvent.SequenceEvent],
        arbitrary[ExecutionEvent.StepEvent]
      )
    }

  given Cogen[ExecutionEvent] =
    Cogen[(
      Option[ExecutionEvent.DatasetEvent],
      Option[ExecutionEvent.SequenceEvent],
      Option[ExecutionEvent.StepEvent]
    )].contramap { a => (
      ExecutionEvent.datasetEvent.getOption(a),
      ExecutionEvent.sequenceEvent.getOption(a),
      ExecutionEvent.stepEvent.getOption(a)
    )}
}

object ArbExecutionEvent extends ArbExecutionEvent

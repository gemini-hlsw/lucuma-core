// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import lucuma.core.enums.AtomStage
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.SlewStage
import lucuma.core.enums.StepStage
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.util.IdempotencyKey
import lucuma.core.util.Timestamp
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbGid
import lucuma.core.util.arb.ArbIdempotencyKey
import lucuma.core.util.arb.ArbTimestamp
import lucuma.core.util.arb.ArbUid
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

trait ArbExecutionEvent:

  import ArbEnumerated.given
  import ArbGid.given
  import ArbIdempotencyKey.given
  import ArbTimestamp.given
  import ArbUid.given

  given Arbitrary[ExecutionEvent.DatasetEvent] =
    Arbitrary:
      for
        eid <- arbitrary[ExecutionEvent.Id]
        rec <- arbitrary[Timestamp]
        oid <- arbitrary[Observation.Id]
        vid <- arbitrary[Visit.Id]
        aid <- arbitrary[Atom.Id]
        idm <- arbitrary[Option[IdempotencyKey]]
        sid <- arbitrary[Step.Id]
        did <- arbitrary[Dataset.Id]
        stg <- arbitrary[DatasetStage]
      yield ExecutionEvent.DatasetEvent(eid, rec, oid, vid, idm, aid, sid, did, stg)

  given Cogen[ExecutionEvent.DatasetEvent] =
    Cogen[(
      ExecutionEvent.Id,
      Timestamp,
      Observation.Id,
      Visit.Id,
      Option[IdempotencyKey],
      Atom.Id,
      Step.Id,
      Dataset.Id,
      DatasetStage
    )].contramap { a => (
      a.id,
      a.received,
      a.observationId,
      a.visitId,
      a.idempotencyKey,
      a.atomId,
      a.stepId,
      a.datasetId,
      a.stage
    )}

  given Arbitrary[ExecutionEvent.SequenceEvent] =
    Arbitrary:
      for
        eid <- arbitrary[ExecutionEvent.Id]
        rec <- arbitrary[Timestamp]
        oid <- arbitrary[Observation.Id]
        vid <- arbitrary[Visit.Id]
        idm <- arbitrary[Option[IdempotencyKey]]
        cmd <- arbitrary[SequenceCommand]
      yield ExecutionEvent.SequenceEvent(eid, rec, oid, vid, idm, cmd)

  given Cogen[ExecutionEvent.SequenceEvent] =
    Cogen[(
      ExecutionEvent.Id,
      Timestamp,
      Observation.Id,
      Visit.Id,
      Option[IdempotencyKey],
      SequenceCommand
    )].contramap { a => (
      a.id,
      a.received,
      a.observationId,
      a.visitId,
      a.idempotencyKey,
      a.command
    )}

  given Arbitrary[ExecutionEvent.SlewEvent] =
    Arbitrary:
      for
        eid <- arbitrary[ExecutionEvent.Id]
        rec <- arbitrary[Timestamp]
        oid <- arbitrary[Observation.Id]
        vid <- arbitrary[Visit.Id]
        idm <- arbitrary[Option[IdempotencyKey]]
        stg <- arbitrary[SlewStage]
      yield ExecutionEvent.SlewEvent(eid, rec, oid, vid, idm, stg)

  given Cogen[ExecutionEvent.SlewEvent] =
    Cogen[(
      ExecutionEvent.Id,
      Timestamp,
      Observation.Id,
      Visit.Id,
      Option[IdempotencyKey],
      SlewStage
    )].contramap { a => (
      a.id,
      a.received,
      a.observationId,
      a.visitId,
      a.idempotencyKey,
      a.stage
    )}

  given Arbitrary[ExecutionEvent.AtomEvent] =
    Arbitrary:
      for
        eid <- arbitrary[ExecutionEvent.Id]
        rec <- arbitrary[Timestamp]
        oid <- arbitrary[Observation.Id]
        vid <- arbitrary[Visit.Id]
        idm <- arbitrary[Option[IdempotencyKey]]
        aid <- arbitrary[Atom.Id]
        stg <- arbitrary[AtomStage]
      yield ExecutionEvent.AtomEvent(eid, rec, oid, vid, idm, aid, stg)

  given Cogen[ExecutionEvent.AtomEvent] =
    Cogen[(
      ExecutionEvent.Id,
      Timestamp,
      Observation.Id,
      Visit.Id,
      Option[IdempotencyKey],
      Atom.Id,
      AtomStage
    )].contramap { a => (
      a.id,
      a.received,
      a.observationId,
      a.visitId,
      a.idempotencyKey,
      a.atomId,
      a.stage
    )}

  given Arbitrary[ExecutionEvent.StepEvent] =
    Arbitrary:
      for
        eid <- arbitrary[ExecutionEvent.Id]
        rec <- arbitrary[Timestamp]
        oid <- arbitrary[Observation.Id]
        vid <- arbitrary[Visit.Id]
        aid <- arbitrary[Atom.Id]
        idm <- arbitrary[Option[IdempotencyKey]]
        sid <- arbitrary[Step.Id]
        stg <- arbitrary[StepStage]
      yield ExecutionEvent.StepEvent(eid, rec, oid, vid, idm, aid, sid, stg)

  given Cogen[ExecutionEvent.StepEvent] =
    Cogen[(
      ExecutionEvent.Id,
      Timestamp,
      Observation.Id,
      Visit.Id,
      Option[IdempotencyKey],
      Atom.Id,
      Step.Id,
      StepStage
    )].contramap { a => (
      a.id,
      a.received,
      a.observationId,
      a.visitId,
      a.idempotencyKey,
      a.atomId,
      a.stepId,
      a.stage
    )}

  given Arbitrary[ExecutionEvent] =
    Arbitrary {
      Gen.oneOf[ExecutionEvent](
        arbitrary[ExecutionEvent.DatasetEvent],
        arbitrary[ExecutionEvent.SequenceEvent],
        arbitrary[ExecutionEvent.AtomEvent],
        arbitrary[ExecutionEvent.SlewEvent],
        arbitrary[ExecutionEvent.StepEvent]
      )
    }

  given Cogen[ExecutionEvent] =
    Cogen[(
      Option[ExecutionEvent.DatasetEvent],
      Option[ExecutionEvent.SequenceEvent],
      Option[ExecutionEvent.AtomEvent],
      Option[ExecutionEvent.SlewEvent],
      Option[ExecutionEvent.StepEvent]
    )].contramap { a => (
      ExecutionEvent.datasetEvent.getOption(a),
      ExecutionEvent.sequenceEvent.getOption(a),
      ExecutionEvent.atomEvent.getOption(a),
      ExecutionEvent.slewEvent.getOption(a),
      ExecutionEvent.stepEvent.getOption(a)
    )}

object ArbExecutionEvent extends ArbExecutionEvent

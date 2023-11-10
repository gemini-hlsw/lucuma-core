// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.syntax.eq.*
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.StepStage
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.util.Timestamp
import lucuma.core.util.WithGid
import lucuma.refined.*
import monocle.Prism
import monocle.macros.GenPrism

sealed trait ExecutionEvent extends Product with Serializable {

  def id:            ExecutionEvent.Id
  def received:      Timestamp
  def observationId: Observation.Id
  def visitId:       Visit.Id

  import ExecutionEvent.*

  def fold[A](
    sequenceEvent: SequenceEvent => A,
    stepEvent:     StepEvent     => A,
    datasetEvent:  DatasetEvent  => A
  ): A =
    this match {
      case e@SequenceEvent(_, _, _, _, _)      => sequenceEvent(e)
      case e@StepEvent(_, _, _, _, _, _)       => stepEvent(e)
      case e@DatasetEvent(_, _, _, _, _, _, _) => datasetEvent(e)
    }

}

object ExecutionEvent extends WithGid('e'.refined) {

  case class SequenceEvent(
    id:            ExecutionEvent.Id,
    received:      Timestamp,
    observationId: Observation.Id,
    visitId:       Visit.Id,
    command:       SequenceCommand
  ) extends ExecutionEvent

  object SequenceEvent {

    given Eq[SequenceEvent] =
      Eq.by { a => (
        a.id,
        a.received,
        a.observationId,
        a.visitId,
        a.command
      )}

  }

  case class StepEvent(
    id:            ExecutionEvent.Id,
    received:      Timestamp,
    observationId: Observation.Id,
    visitId:       Visit.Id,
    stepId:        Step.Id,
    stage:         StepStage
  ) extends ExecutionEvent

  object StepEvent {

    given Eq[StepEvent] =
      Eq.by { a => (
        a.id,
        a.received,
        a.observationId,
        a.visitId,
        a.stepId,
        a.stage
      )}

  }

  case class DatasetEvent(
    id:            ExecutionEvent.Id,
    received:      Timestamp,
    observationId: Observation.Id,
    visitId:       Visit.Id,
    stepId:        Step.Id,
    datasetId:     Dataset.Id,
    stage:         DatasetStage
  ) extends ExecutionEvent

  object DatasetEvent {

    given Eq[DatasetEvent] =
      Eq.by { a => (
        a.id,
        a.received,
        a.observationId,
        a.visitId,
        a.stepId,
        a.datasetId,
        a.stage
      )}

  }

  given Eq[ExecutionEvent] =
    Eq.instance {
      case (e0: SequenceEvent, e1: SequenceEvent) => e0 === e1
      case (e0: StepEvent, e1: StepEvent)         => e0 === e1
      case (e0: DatasetEvent, e1: DatasetEvent)   => e0 === e1
      case _ => false
    }

  val datasetEvent: Prism[ExecutionEvent, DatasetEvent] =
    GenPrism[ExecutionEvent, DatasetEvent]

  val sequenceEvent: Prism[ExecutionEvent, SequenceEvent] =
    GenPrism[ExecutionEvent, SequenceEvent]

  val stepEvent: Prism[ExecutionEvent, StepEvent] =
    GenPrism[ExecutionEvent, StepEvent]

}

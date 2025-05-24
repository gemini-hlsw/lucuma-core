// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.derived.*
import lucuma.core.enums.AtomStage
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.SlewStage
import lucuma.core.enums.StepStage
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.util.Timestamp
import lucuma.core.util.WithGid
import lucuma.refined.*
import monocle.Prism
import monocle.macros.GenPrism

sealed trait ExecutionEvent derives Eq {

  def id:            ExecutionEvent.Id
  def received:      Timestamp
  def observationId: Observation.Id
  def visitId:       Visit.Id

  import ExecutionEvent.*

  def fold[A](
    slewEvent:     SlewEvent     => A,
    sequenceEvent: SequenceEvent => A,
    atomEvent:     AtomEvent     => A,
    stepEvent:     StepEvent     => A,
    datasetEvent:  DatasetEvent  => A
  ): A =
    this match {
      case e@SlewEvent(_, _, _, _, _)             => slewEvent(e)
      case e@SequenceEvent(_, _, _, _, _)         => sequenceEvent(e)
      case e@AtomEvent(_, _, _, _, _, _)          => atomEvent(e)
      case e@StepEvent(_, _, _, _, _, _, _)       => stepEvent(e)
      case e@DatasetEvent(_, _, _, _, _, _, _, _) => datasetEvent(e)
    }

}

object ExecutionEvent extends WithGid('e'.refined) {

  case class SlewEvent(
    id:            ExecutionEvent.Id,
    received:      Timestamp,
    observationId: Observation.Id,
    visitId:       Visit.Id,
    stage:         SlewStage
  ) extends ExecutionEvent derives Eq

  case class SequenceEvent(
    id:            ExecutionEvent.Id,
    received:      Timestamp,
    observationId: Observation.Id,
    visitId:       Visit.Id,
    command:       SequenceCommand
  ) extends ExecutionEvent derives Eq

  case class AtomEvent(
    id:            ExecutionEvent.Id,
    received:      Timestamp,
    observationId: Observation.Id,
    visitId:       Visit.Id,
    atomId:        Atom.Id,
    stage:         AtomStage
  ) extends ExecutionEvent derives Eq

  case class StepEvent(
    id:            ExecutionEvent.Id,
    received:      Timestamp,
    observationId: Observation.Id,
    visitId:       Visit.Id,
    atomId:        Atom.Id,
    stepId:        Step.Id,
    stage:         StepStage
  ) extends ExecutionEvent derives Eq

  case class DatasetEvent(
    id:            ExecutionEvent.Id,
    received:      Timestamp,
    observationId: Observation.Id,
    visitId:       Visit.Id,
    atomId:        Atom.Id,
    stepId:        Step.Id,
    datasetId:     Dataset.Id,
    stage:         DatasetStage
  ) extends ExecutionEvent derives Eq

  val atomEvent: Prism[ExecutionEvent, AtomEvent] =
    GenPrism[ExecutionEvent, AtomEvent]

  val datasetEvent: Prism[ExecutionEvent, DatasetEvent] =
    GenPrism[ExecutionEvent, DatasetEvent]

  val sequenceEvent: Prism[ExecutionEvent, SequenceEvent] =
    GenPrism[ExecutionEvent, SequenceEvent]

  val slewEvent: Prism[ExecutionEvent, SlewEvent] =
    GenPrism[ExecutionEvent, SlewEvent]

  val stepEvent: Prism[ExecutionEvent, StepEvent] =
    GenPrism[ExecutionEvent, StepEvent]

}

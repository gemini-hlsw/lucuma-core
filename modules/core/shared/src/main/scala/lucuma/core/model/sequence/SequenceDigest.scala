// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.Monoid
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.ObserveClass
import lucuma.core.model.sequence.exposure.ExposureTimeViolation
import monocle.Focus
import monocle.Lens

import scala.collection.immutable.SortedSet

/**
 * A compilation of attributes about a sequence computed from its atoms.
 *
 * @param observeClass   ObserveClass of the sequence as a whole
 * @param plannedTime    expected execution time for the sequence
 * @param configs        set of offsets and guide states that are expected over the
 *                       course of the sequence execution
 * @param atomCount      number of atoms in the sequence
 * @param gcalSets       number of atoms that contain at least one GCAL step
 * @param steps          count and estimated time of the steps, by kind
 * @param executionState completion state for this sequence
 * @param exposureTimeViolations the exposure rules that steps of the
 *                       sequence violate
 */
case class SequenceDigest(
  observeClass:           ObserveClass,
  timeEstimate:           CategorizedTime,
  telescopeConfigs:       SortedSet[TelescopeConfig],
  atomCount:              NonNegInt,
  gcalSets:               NonNegInt,
  steps:                  StepDigests,
  executionState:         ExecutionState,
  exposureTimeViolations: SortedSet[ExposureTimeViolation]
)

object SequenceDigest:

  val Zero: SequenceDigest =
    SequenceDigest(
      Monoid[ObserveClass].empty,
      CategorizedTime.Zero,
      SortedSet.empty,
      NonNegInt.unsafeFrom(0),
      NonNegInt.unsafeFrom(0),
      StepDigests.Zero,
      ExecutionState.NotStarted,
      SortedSet.empty
    )

  /** @group Optics */
  val observeClass: Lens[SequenceDigest, ObserveClass] =
    Focus[SequenceDigest](_.observeClass)

  /** @group Optics */
  val timeEstimate: Lens[SequenceDigest, CategorizedTime] =
    Focus[SequenceDigest](_.timeEstimate)

  /** @group Optics */
  val configs: Lens[SequenceDigest, SortedSet[TelescopeConfig]] =
    Focus[SequenceDigest](_.telescopeConfigs)

  /** @group Optics */
  val steps: Lens[SequenceDigest, StepDigests] =
    Focus[SequenceDigest](_.steps)

  /** @group Optics */
  val executionState: Lens[SequenceDigest, ExecutionState] =
    Focus[SequenceDigest](_.executionState)

  /** @group Optics */
  val atomCount: Lens[SequenceDigest, NonNegInt] =
    Focus[SequenceDigest](_.atomCount)

  /** @group Optics */
  val gcalSets: Lens[SequenceDigest, NonNegInt] =
    Focus[SequenceDigest](_.gcalSets)

  /** @group Optics */
  val exposureTimeViolations: Lens[SequenceDigest, SortedSet[ExposureTimeViolation]] =
    Focus[SequenceDigest](_.exposureTimeViolations)

  given Eq[SequenceDigest] =
    Eq.by: a =>
      (
        a.observeClass,
        a.timeEstimate,
        a.telescopeConfigs,
        a.atomCount,
        a.gcalSets,
        a.steps,
        a.executionState,
        a.exposureTimeViolations
      )

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.Monoid
import cats.syntax.monoid.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.GcalLampType
import lucuma.core.enums.ObserveClass
import lucuma.core.util.TimeSpan
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
 * @param arcs           count and estimated time of the arc steps
 * @param flats          count and estimated time of the flat steps
 * @param executionState completion state for this sequence
 */
case class SequenceDigest(
  observeClass:     ObserveClass,
  timeEstimate:     CategorizedTime,
  telescopeConfigs: SortedSet[TelescopeConfig],
  atomCount:        NonNegInt,
  arcs:             GcalDigest,
  flats:            GcalDigest,
  executionState:   ExecutionState
):

  def add[D](a: Atom[D]): SequenceDigest =
    SequenceDigest(
      observeClass     = observeClass |+| a.observeClass,
      timeEstimate     = timeEstimate |+| a.timeEstimate,
      telescopeConfigs = telescopeConfigs ++ a.steps.toList.map(_.telescopeConfig),
      atomCount        = NonNegInt.unsafeFrom(atomCount.value + 1),
      arcs             = gcalSteps(a, GcalLampType.Arc).foldLeft(arcs)(_.add(_)),
      flats            = gcalSteps(a, GcalLampType.Flat).foldLeft(flats)(_.add(_)),
      executionState   = executionState
    )

  private def gcalSteps[D](a: Atom[D], lampType: GcalLampType): List[TimeSpan] =
    a.steps.toList.collect:
      case s if s.stepConfig.gcalLampType.contains(lampType) => s.estimate.total

object SequenceDigest:

  val Zero: SequenceDigest =
    SequenceDigest(
      Monoid[ObserveClass].empty,
      CategorizedTime.Zero,
      SortedSet.empty,
      NonNegInt.unsafeFrom(0),
      GcalDigest.Zero,
      GcalDigest.Zero,
      ExecutionState.NotStarted
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
  val arcs: Lens[SequenceDigest, GcalDigest] =
    Focus[SequenceDigest](_.arcs)

  /** @group Optics */
  val flats: Lens[SequenceDigest, GcalDigest] =
    Focus[SequenceDigest](_.flats)

  /** @group Optics */
  val executionState: Lens[SequenceDigest, ExecutionState] =
    Focus[SequenceDigest](_.executionState)

  /** @group Optics */
  val atomCount: Lens[SequenceDigest, NonNegInt] =
    Focus[SequenceDigest](_.atomCount)

  given Eq[SequenceDigest] =
    Eq.by: a =>
      (
        a.observeClass,
        a.timeEstimate,
        a.telescopeConfigs,
        a.atomCount,
        a.arcs,
        a.flats,
        a.executionState
      )

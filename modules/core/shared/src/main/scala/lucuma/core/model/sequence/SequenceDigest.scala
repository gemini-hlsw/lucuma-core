// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.Monoid
import cats.syntax.monoid.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.StepType
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
 * @param biases         count and estimated time of the bias steps
 * @param darks          count and estimated time of the dark steps
 * @param arcs           count and estimated time of the arc steps
 * @param flats          count and estimated time of the flat steps
 * @param observing      count and estimated time of all other steps
 * @param executionState completion state for this sequence
 */
case class SequenceDigest(
  observeClass:     ObserveClass,
  timeEstimate:     CategorizedTime,
  telescopeConfigs: SortedSet[TelescopeConfig],
  atomCount:        NonNegInt,
  biases:           StepDigest,
  darks:            StepDigest,
  arcs:             StepDigest,
  flats:            StepDigest,
  observing:        StepDigest,
  executionState:   ExecutionState
):

  def add[D](a: Atom[D]): SequenceDigest =
    val init = (biases, darks, arcs, flats, observing)
    val (b, d, r, f, o) =
      a.steps.toList.foldLeft(init): (acc, s) =>
        val (b, d, r, f, o) = acc
        val t = s.timeEstimate
        s.stepConfig.stepType match
          case StepType.Bias => (b.add(t), d, r, f, o)
          case StepType.Dark => (b, d.add(t), r, f, o)
          case StepType.Gcal | StepType.SmartGcal =>
            // A GCAL step is an arc or a flat; an unrecognised lamp counts as a flat.
            if s.stepConfig.isArc then (b, d, r.add(t), f, o) else (b, d, r, f.add(t), o)
          case StepType.Science => (b, d, r, f, o.add(t))

    SequenceDigest(
      observeClass     = observeClass |+| a.observeClass,
      timeEstimate     = timeEstimate |+| a.timeEstimate,
      telescopeConfigs = telescopeConfigs ++ a.steps.toList.map(_.telescopeConfig),
      atomCount        = NonNegInt.unsafeFrom(atomCount.value + 1),
      biases           = b,
      darks            = d,
      arcs             = r,
      flats            = f,
      observing        = o,
      executionState   = executionState
    )

object SequenceDigest:

  val Zero: SequenceDigest =
    SequenceDigest(
      Monoid[ObserveClass].empty,
      CategorizedTime.Zero,
      SortedSet.empty,
      NonNegInt.unsafeFrom(0),
      StepDigest.Zero,
      StepDigest.Zero,
      StepDigest.Zero,
      StepDigest.Zero,
      StepDigest.Zero,
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
  val biases: Lens[SequenceDigest, StepDigest] =
    Focus[SequenceDigest](_.biases)

  /** @group Optics */
  val darks: Lens[SequenceDigest, StepDigest] =
    Focus[SequenceDigest](_.darks)

  /** @group Optics */
  val arcs: Lens[SequenceDigest, StepDigest] =
    Focus[SequenceDigest](_.arcs)

  /** @group Optics */
  val flats: Lens[SequenceDigest, StepDigest] =
    Focus[SequenceDigest](_.flats)

  /** @group Optics */
  val observing: Lens[SequenceDigest, StepDigest] =
    Focus[SequenceDigest](_.observing)

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
        a.biases,
        a.darks,
        a.arcs,
        a.flats,
        a.observing,
        a.executionState
      )

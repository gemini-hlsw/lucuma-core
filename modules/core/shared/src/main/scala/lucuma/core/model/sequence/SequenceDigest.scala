// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.Monoid
import cats.Order.catsKernelOrderingForOrder
import cats.syntax.monoid.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.ObserveClass
import lucuma.core.math.Offset
import monocle.Focus
import monocle.Lens

import scala.collection.immutable.SortedSet

/**
 * A compilation of attributes about a sequence computed from its atoms.
 *
 * @param observeClass   ObserveClass of the sequence as a whole
 * @param plannedTime    expected execution time for the sequence
 * @param offsets        set of offsets that are expected over the course of the
 *                       sequence execution
 * @param atomCount      number of atoms in the sequence
 * @param executionState completion state for this sequence
 */
case class SequenceDigest(
  observeClass:   ObserveClass,
  timeEstimate:   CategorizedTime,
  offsets:        SortedSet[Offset],
  atomCount:      NonNegInt,
  executionState: ExecutionState
):

  def add[D](a: Atom[D]): SequenceDigest =
    SequenceDigest(
      observeClass   = observeClass |+| a.observeClass,
      timeEstimate   = timeEstimate |+| a.timeEstimate,
      offsets        = a.steps.toList.foldLeft(offsets)((os, s) => os + s.telescopeConfig.offset),
      atomCount      = NonNegInt.unsafeFrom(atomCount.value + 1),
      executionState = executionState
    )

object SequenceDigest:

  val Zero: SequenceDigest =
    SequenceDigest(
      Monoid[ObserveClass].empty,
      CategorizedTime.Zero,
      SortedSet.empty,
      NonNegInt.unsafeFrom(0),
      ExecutionState.NotStarted
    )

  /** @group Optics */
  val observeClass: Lens[SequenceDigest, ObserveClass] =
    Focus[SequenceDigest](_.observeClass)

  /** @group Optics */
  val timeEstimate: Lens[SequenceDigest, CategorizedTime] =
    Focus[SequenceDigest](_.timeEstimate)

  /** @group Optics */
  val offsets: Lens[SequenceDigest, SortedSet[Offset]] =
    Focus[SequenceDigest](_.offsets)

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
        a.offsets,
        a.atomCount,
        a.executionState
      )
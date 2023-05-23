// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.Monoid
import cats.Order.catsKernelOrderingForOrder
import cats.syntax.monoid.*
import lucuma.core.enums.ObserveClass
import lucuma.core.math.Offset
import monocle.Focus
import monocle.Lens

import scala.collection.immutable.SortedSet

/**
 * A compilation of attributes about a sequence computed from its atoms.
 *
 * @param observeClass ObserveClass of the sequence as a whole
 * @param plannedTime  expected execution time for the sequence
 * @param offsets      set of offsets that are expected over the course of the
 *                     sequence execution
 */
case class SequenceDigest(
  observeClass: ObserveClass,
  plannedTime:  PlannedTime,
  offsets:      SortedSet[Offset]
) {

  def add(o: ObserveClass): SequenceDigest =
    SequenceDigest.observeClass.modify(_ |+| o)(this)

  def add(p: PlannedTime): SequenceDigest =
    SequenceDigest.plannedTime.modify(_ |+| p)(this)

  def add(o: Offset): SequenceDigest =
    SequenceDigest.offsets.modify(_ + o)(this)

  def add(d: SequenceDigest): SequenceDigest =
    SequenceDigest(
      observeClass |+| d.observeClass,
      plannedTime  |+| d.plannedTime,
      offsets.union(d.offsets)
    )

}

object SequenceDigest {

  val Zero: SequenceDigest =
    SequenceDigest(
      Monoid[ObserveClass].empty,
      PlannedTime.Zero,
      SortedSet.empty
    )

  /** @group Optics */
  def observeClass: Lens[SequenceDigest, ObserveClass] =
    Focus[SequenceDigest](_.observeClass)

  /** @group Optics */
  def plannedTime: Lens[SequenceDigest, PlannedTime] =
    Focus[SequenceDigest](_.plannedTime)

  /** @group Optics */
  def offsets: Lens[SequenceDigest, SortedSet[Offset]] =
    Focus[SequenceDigest](_.offsets)

  given Monoid[SequenceDigest] with {
    def empty: SequenceDigest =
      Zero

    def combine(a: SequenceDigest, b: SequenceDigest): SequenceDigest =
      a.add(b)
  }

  given Eq[SequenceDigest] =
    Eq.by { a => (
      a.observeClass,
      a.plannedTime,
      a.offsets
    )}

}


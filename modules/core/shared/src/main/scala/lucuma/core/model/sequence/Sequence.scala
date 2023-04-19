// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.all._
import lucuma.core.data.Zipper
import lucuma.core.enums.ObserveClass
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

/**
 * A Sequence is a list of Atom.  An observation will have acquisition and
 * science sequences.
 *
 * @tparam D dynamic config type
 */
case class Sequence[D](
  atoms: NonEmptyList[Atom[D]]
) {

  lazy val observeClass: ObserveClass =
    atoms.foldMap(_.observeClass)

  lazy val plannedTime: PlannedTime =
    atoms.foldMap(_.plannedTime)

}

object Sequence {

  /** @group Optics */
  def atoms[D]: Lens[Sequence[D], NonEmptyList[Atom[D]]] =
    Focus[Sequence[D]](_.atoms)

  given [D](using Eq[D]): Eq[Sequence[D]] =
    Eq.by(_.atoms)

}


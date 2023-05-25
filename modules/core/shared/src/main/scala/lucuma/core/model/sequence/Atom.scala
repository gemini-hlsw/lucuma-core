// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.all._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.data.Zipper
import lucuma.core.enums.ObserveClass
import lucuma.core.util.TimeSpan
import lucuma.core.util.WithUid
import lucuma.refined._
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

/**
 * An Atom is a collection of steps that must be treated as indivisible
 * at execution time.
 *
 * @tparam D dynamic config type (e.g., DynamicConfig.GmosNorth)
 */
case class Atom[D](
  id:          Atom.Id,
  description: Option[NonEmptyString],
  steps:       NonEmptyList[Step[D]]
) {

  lazy val observeClass: ObserveClass =
    steps.foldMap(_.observeClass)

  lazy val plannedTime: PlannedTime =
    steps.foldMap(_.plannedTime)

}

object Atom extends WithUid('a'.refined) {

  /** @group Optics */
  def id[D]: Lens[Atom[D], Atom.Id] =
    Focus[Atom[D]](_.id)

  /** @group Optics */
  def description[D]: Lens[Atom[D], Option[NonEmptyString]] =
    Focus[Atom[D]](_.description)

  /** @group Optics */
  def steps[D]: Lens[Atom[D], NonEmptyList[Step[D]]] =
    Focus[Atom[D]](_.steps)

  given [D](using Eq[D]): Eq[Atom[D]] =
    Eq.by { a => (
      a.id,
      a.description.map(_.value),
      a.steps
    )}
}


// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.syntax.all._
import eu.timepit.refined.auto._
import lucuma.core.util.WithUid
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

sealed trait Atom {
  val id: Atom.Id
  val steps: List[Step]
  val time: StepTime
}

object Atom extends WithUid('a') {
  final case class GmosNorth(id: Atom.Id, steps: List[Step.GmosNorth], time: StepTime) extends Atom
  object GmosNorth {
    implicit val eqAtomGmosNorth: Eq[GmosNorth] = Eq.by(a => (a.id, a.steps, a.time))

    /** @group Optics */
    val id: Lens[GmosNorth, Atom.Id] =
      Focus[GmosNorth](_.id)

    /** @group Optics */
    val steps: Lens[GmosNorth, List[Step.GmosNorth]] =
      Focus[GmosNorth](_.steps)

    /** @group Optics */
    val time: Lens[GmosNorth, StepTime] =
      Focus[GmosNorth](_.time)
  }

  final case class GmosSouth(id: Atom.Id, steps: List[Step.GmosSouth], time: StepTime) extends Atom
  object GmosSouth {
    implicit val eqAtomGmosSouth: Eq[GmosSouth] = Eq.by(a => (a.id, a.steps, a.time))

    /** @group Optics */
    val id: Lens[GmosSouth, Atom.Id] =
      Focus[GmosSouth](_.id)

    /** @group Optics */
    val steps: Lens[GmosSouth, List[Step.GmosSouth]] =
      Focus[GmosSouth](_.steps)

    /** @group Optics */
    val time: Lens[GmosSouth, StepTime] =
      Focus[GmosSouth](_.time)
  }

  implicit val eqAtom: Eq[Atom] = Eq.instance {
    case (a @ GmosNorth(_, _, _), b @ GmosNorth(_, _, _)) => a === b
    case (a @ GmosSouth(_, _, _), b @ GmosSouth(_, _, _)) => a === b
    case _                                                => false
  }

  /** @group Optics */
  val gmosNorth: Prism[Atom, GmosNorth] =
    GenPrism[Atom, GmosNorth]

  /** @group Optics */
  val gmosSouth: Prism[Atom, GmosSouth] =
    GenPrism[Atom, GmosSouth]
}

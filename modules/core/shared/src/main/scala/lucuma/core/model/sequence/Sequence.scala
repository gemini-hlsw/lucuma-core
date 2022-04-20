// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.syntax.all._
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

sealed trait Sequence {
  val atoms: List[Atom]
  val time: StepTime
}
object Sequence       {
  final case class GmosNorth(atoms: List[Atom.GmosNorth], time: StepTime) extends Sequence
  object GmosNorth {
    implicit val eqSequenceGmosNorth: Eq[GmosNorth] = Eq.by(x => (x.atoms, x.time))

    /** @group Optics */
    val atoms: Lens[GmosNorth, List[Atom.GmosNorth]] =
      Focus[GmosNorth](_.atoms)

    /** @group Optics */
    val time: Lens[GmosNorth, StepTime] =
      Focus[GmosNorth](_.time)
  }

  final case class GmosSouth(atoms: List[Atom.GmosSouth], time: StepTime) extends Sequence
  object GmosSouth {
    implicit val eqSequenceGmosSouth: Eq[GmosSouth] = Eq.by(x => (x.atoms, x.time))

    /** @group Optics */
    val atoms: Lens[GmosSouth, List[Atom.GmosSouth]] =
      Focus[GmosSouth](_.atoms)

    /** @group Optics */
    val time: Lens[GmosSouth, StepTime] =
      Focus[GmosSouth](_.time)
  }

  implicit val eqSequence: Eq[Sequence] = Eq.instance {
    case (a @ GmosNorth(_, _), b @ GmosNorth(_, _)) => a === b
    case (a @ GmosSouth(_, _), b @ GmosSouth(_, _)) => a === b
    case _                                          => false
  }

  /** @group Optics */
  val gmosNorth: Prism[Sequence, GmosNorth] =
    GenPrism[Sequence, GmosNorth]

  /** @group Optics */
  val gmosSouth: Prism[Sequence, GmosSouth] =
    GenPrism[Sequence, GmosSouth]
}

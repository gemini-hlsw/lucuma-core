// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.syntax.all._
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

sealed trait ExecutionSequence {
  val nextAtom: Atom
  val possibleFuture: List[Atom]
}

object ExecutionSequence {
  final case class GmosNorth(nextAtom: Atom.GmosNorth, possibleFuture: List[Atom.GmosNorth])
      extends ExecutionSequence
  object GmosNorth {
    implicit val eqExecutionSequenceGmosNorth: Eq[GmosNorth] =
      Eq.by(x => (x.nextAtom, x.possibleFuture))

    /** @group Optics */
    val nextAtom: Lens[GmosNorth, Atom.GmosNorth] =
      Focus[GmosNorth](_.nextAtom)

    /** @group Optics */
    val possibleFuture: Lens[GmosNorth, List[Atom.GmosNorth]] =
      Focus[GmosNorth](_.possibleFuture)
  }

  final case class GmosSouth(nextAtom: Atom.GmosSouth, possibleFuture: List[Atom.GmosSouth])
      extends ExecutionSequence
  object GmosSouth {
    implicit val eqExecutionSequenceGmosSouth: Eq[GmosSouth] =
      Eq.by(x => (x.nextAtom, x.possibleFuture))

    /** @group Optics */
    val nextAtom: Lens[GmosSouth, Atom.GmosSouth] =
      Focus[GmosSouth](_.nextAtom)

    /** @group Optics */
    val possibleFuture: Lens[GmosSouth, List[Atom.GmosSouth]] =
      Focus[GmosSouth](_.possibleFuture)
  }

  implicit val eqExecutionSequence: Eq[ExecutionSequence] = Eq.instance {
    case (a @ GmosNorth(_, _), b @ GmosNorth(_, _)) => a === b
    case (a @ GmosSouth(_, _), b @ GmosSouth(_, _)) => a === b
    case _                                          => false
  }

  /** @group Optics */
  val gmosNorth: Prism[ExecutionSequence, GmosNorth] =
    GenPrism[ExecutionSequence, GmosNorth]

  /** @group Optics */
  val gmosSouth: Prism[ExecutionSequence, GmosSouth] =
    GenPrism[ExecutionSequence, GmosSouth]
}

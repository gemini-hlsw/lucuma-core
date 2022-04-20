// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import cats.syntax.all._
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Sequence
import lucuma.core.model.sequence.StepTime
import lucuma.core.util.arb.ArbUid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbSequence {
  import ArbUid._
  import ArbAtom._
  import ArbStepTime._

  implicit val arbSequenceGmosNorth: Arbitrary[Sequence.GmosNorth] = Arbitrary(
    for {
      atoms <- arbitrary[List[Atom.GmosNorth]]
      time  <- arbitrary[StepTime]
    } yield Sequence.GmosNorth(atoms, time)
  )

  implicit val cogSequenceGmosNorth: Cogen[Sequence.GmosNorth] =
    Cogen[(List[Atom.GmosNorth], StepTime)].contramap(s => (s.atoms, s.time))

  implicit val arbSequenceGmosSouth: Arbitrary[Sequence.GmosSouth] = Arbitrary(
    for {
      atoms <- arbitrary[List[Atom.GmosSouth]]
      time  <- arbitrary[StepTime]
    } yield Sequence.GmosSouth(atoms, time)
  )

  implicit val cogSequenceGmosSouth: Cogen[Sequence.GmosSouth] =
    Cogen[(List[Atom.GmosSouth], StepTime)].contramap(s => (s.atoms, s.time))

  implicit val arbSequence: Arbitrary[Sequence] = Arbitrary(
    Gen.oneOf(
      arbitrary[Sequence.GmosNorth],
      arbitrary[Sequence.GmosSouth]
    )
  )

  implicit val cogSequence: Cogen[Sequence] =
    Cogen[Either[Sequence.GmosNorth, Sequence.GmosSouth]].contramap {
      case s @ Sequence.GmosNorth(_, _) => s.asLeft
      case s @ Sequence.GmosSouth(_, _) => s.asRight
    }
}

object ArbSequence extends ArbSequence

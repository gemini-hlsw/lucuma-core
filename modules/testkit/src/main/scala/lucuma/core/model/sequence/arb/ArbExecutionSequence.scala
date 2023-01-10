// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import cats.syntax.all._
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionSequence
import lucuma.core.util.arb.ArbUid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbExecutionSequence {
  import ArbUid._
  import ArbAtom._

  implicit val arbExecutionSequenceGmosNorth: Arbitrary[ExecutionSequence.GmosNorth] = Arbitrary(
    for {
      nextAtom       <- arbitrary[Atom.GmosNorth]
      possibleFuture <- arbitrary[List[Atom.GmosNorth]]
    } yield ExecutionSequence.GmosNorth(nextAtom, possibleFuture)
  )

  implicit val cogExecutionSequenceGmosNorth: Cogen[ExecutionSequence.GmosNorth] =
    Cogen[(Atom.GmosNorth, List[Atom.GmosNorth])].contramap(s => (s.nextAtom, s.possibleFuture))

  implicit val arbExecutionSequenceGmosSouth: Arbitrary[ExecutionSequence.GmosSouth] = Arbitrary(
    for {
      nextAtom       <- arbitrary[Atom.GmosSouth]
      possibleFuture <- arbitrary[List[Atom.GmosSouth]]
    } yield ExecutionSequence.GmosSouth(nextAtom, possibleFuture)
  )

  implicit val cogExecutionSequenceGmosSouth: Cogen[ExecutionSequence.GmosSouth] =
    Cogen[(Atom.GmosSouth, List[Atom.GmosSouth])].contramap(s => (s.nextAtom, s.possibleFuture))

  implicit val arbExecutionSequence: Arbitrary[ExecutionSequence] = Arbitrary(
    Gen.oneOf(
      arbitrary[ExecutionSequence.GmosNorth],
      arbitrary[ExecutionSequence.GmosSouth]
    )
  )

  implicit val cogExecutionSequence: Cogen[ExecutionSequence] =
    Cogen[Either[ExecutionSequence.GmosNorth, ExecutionSequence.GmosSouth]].contramap {
      case s @ ExecutionSequence.GmosNorth(_, _) => s.asLeft
      case s @ ExecutionSequence.GmosSouth(_, _) => s.asRight
    }
}

object ArbExecutionSequence extends ArbExecutionSequence

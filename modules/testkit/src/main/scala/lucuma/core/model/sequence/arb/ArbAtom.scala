// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import cats.syntax.all._
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepTime
import lucuma.core.util.arb.ArbUid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbAtom {
  import ArbUid._
  import ArbStep._
  import ArbStepTime._

  implicit val arbAtomGmosNorth: Arbitrary[Atom.GmosNorth] = Arbitrary(
    for {
      id    <- arbitrary[Atom.Id]
      steps <- arbitrary[List[Step.GmosNorth]]
      time  <- arbitrary[StepTime]
    } yield Atom.GmosNorth(id, steps, time)
  )

  implicit val cogAtomGmosNorth: Cogen[Atom.GmosNorth] =
    Cogen[(Atom.Id, List[Step.GmosNorth], StepTime)].contramap(a => (a.id, a.steps, a.time))

  implicit val arbAtomGmosSouth: Arbitrary[Atom.GmosSouth] = Arbitrary(
    for {
      id    <- arbitrary[Atom.Id]
      steps <- arbitrary[List[Step.GmosSouth]]
      time  <- arbitrary[StepTime]
    } yield Atom.GmosSouth(id, steps, time)
  )

  implicit val cogAtomGmosSouth: Cogen[Atom.GmosSouth] =
    Cogen[(Atom.Id, List[Step.GmosSouth], StepTime)].contramap(a => (a.id, a.steps, a.time))

  implicit val arbAtom: Arbitrary[Atom] = Arbitrary(
    Gen.oneOf(
      arbitrary[Atom.GmosNorth],
      arbitrary[Atom.GmosSouth]
    )
  )

  implicit val cogAtom: Cogen[Atom] =
    Cogen[Either[Atom.GmosNorth, Atom.GmosSouth]].contramap {
      case a @ Atom.GmosNorth(_, _, _) => a.asLeft
      case a @ Atom.GmosSouth(_, _, _) => a.asRight
    }
}

object ArbAtom extends ArbAtom

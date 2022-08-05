// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import cats.syntax.all._
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.FutureStep
import lucuma.core.util.arb.ArbUid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbAtom {
  import ArbUid._
  import ArbFutureStep._

  implicit val arbAtomGmosNorth: Arbitrary[Atom.GmosNorth] = Arbitrary(
    for {
      id    <- arbitrary[Atom.Id]
      steps <- arbitrary[List[FutureStep.GmosNorth]]
    } yield Atom.GmosNorth(id, steps)
  )

  implicit val cogAtomGmosNorth: Cogen[Atom.GmosNorth] =
    Cogen[(Atom.Id, List[FutureStep.GmosNorth])].contramap(a => (a.id, a.steps))

  implicit val arbAtomGmosSouth: Arbitrary[Atom.GmosSouth] = Arbitrary(
    for {
      id    <- arbitrary[Atom.Id]
      steps <- arbitrary[List[FutureStep.GmosSouth]]
    } yield Atom.GmosSouth(id, steps)
  )

  implicit val cogAtomGmosSouth: Cogen[Atom.GmosSouth] =
    Cogen[(Atom.Id, List[FutureStep.GmosSouth])].contramap(a => (a.id, a.steps))

  implicit val arbAtom: Arbitrary[Atom] = Arbitrary(
    Gen.oneOf(
      arbitrary[Atom.GmosNorth],
      arbitrary[Atom.GmosSouth]
    )
  )

  implicit val cogAtom: Cogen[Atom] =
    Cogen[Either[Atom.GmosNorth, Atom.GmosSouth]].contramap {
      case a @ Atom.GmosNorth(_, _) => a.asLeft
      case a @ Atom.GmosSouth(_, _) => a.asRight
    }
}

object ArbAtom extends ArbAtom

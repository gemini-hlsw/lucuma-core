// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import cats.syntax.all._
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.util.arb.ArbUid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbAtom {
  import ArbUid._
  import ArbStep._

  private def genBoundedAtom[A: Arbitrary, B](limit: Int, f: (Atom.Id, List[A]) => B): Gen[B] =
    for {
      id    <- arbitrary[Atom.Id]
      steps <- genBoundedList[A](limit)
    } yield f(id, steps)

  def genBoundedAtomGmosNorth(limit: Int): Gen[Atom.GmosNorth] =
    genBoundedAtom[Step.GmosNorth, Atom.GmosNorth](limit, Atom.GmosNorth.apply)

  implicit val arbAtomGmosNorth: Arbitrary[Atom.GmosNorth] =
    Arbitrary(genBoundedAtomGmosNorth(10))

  implicit val cogAtomGmosNorth: Cogen[Atom.GmosNorth] =
    Cogen[(Atom.Id, List[Step.GmosNorth])].contramap(a => (a.id, a.steps))

  def genBoundedAtomGmosSouth(limit: Int): Gen[Atom.GmosSouth] =
    genBoundedAtom[Step.GmosSouth, Atom.GmosSouth](limit, Atom.GmosSouth.apply)

  implicit val arbAtomGmosSouth: Arbitrary[Atom.GmosSouth] =
    Arbitrary(genBoundedAtomGmosSouth(10))

  implicit val cogAtomGmosSouth: Cogen[Atom.GmosSouth] =
    Cogen[(Atom.Id, List[Step.GmosSouth])].contramap(a => (a.id, a.steps))

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

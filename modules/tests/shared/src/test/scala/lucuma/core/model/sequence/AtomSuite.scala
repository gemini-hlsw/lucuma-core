// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline._
import lucuma.core.model.sequence.arb._
import lucuma.core.util.arb._
import lucuma.core.util.laws.UidTests
import monocle.law.discipline._
import munit._
import org.scalacheck.Test

final class AtomSuite extends DisciplineSuite {
  import ArbStep._
  import ArbEnumerated._
  import ArbUid._
  import ArbAtom._

  override val scalaCheckTestParameters = Test.Parameters.default.withMaxSize(20)

  checkAll("Eq[Atom.GmosNorth]", EqTests[Atom.GmosNorth].eqv)
  checkAll("Atom.GmosNorth.id", LensTests(Atom.GmosNorth.id))
  checkAll("Atom.GmosNorth.steps", LensTests(Atom.GmosNorth.steps))

  checkAll("Eq[Atom.GmosSouth]", EqTests[Atom.GmosSouth].eqv)
  checkAll("Atom.GmosSouth.id", LensTests(Atom.GmosSouth.id))
  checkAll("Atom.GmosSouth.steps", LensTests(Atom.GmosSouth.steps))

  checkAll("Eq[Atom]", EqTests[Atom].eqv)
  checkAll("Atom.Id", UidTests[Atom.Id].uid)
  checkAll("Atom.gmosNorth", PrismTests(Atom.gmosNorth))
  checkAll("Atom.gmosSouth", PrismTests(Atom.gmosSouth))
}

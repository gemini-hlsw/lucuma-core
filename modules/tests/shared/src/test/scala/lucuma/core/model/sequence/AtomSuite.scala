// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import lucuma.core.model.sequence.arb.*
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.arb.*
import lucuma.core.util.arb.*
import lucuma.core.util.laws.UidTests
import monocle.law.discipline.*
import munit.*
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Test


final class AtomSuite extends DisciplineSuite {
  import ArbAtom.given
  import ArbBoundedCollection.given
  import ArbDynamicConfig._
  import ArbEnumerated._
  import ArbStep.given
  import ArbUid._

  override val scalaCheckTestParameters = Test.Parameters.default.withMaxSize(10)

  checkAll("Atom.Id", UidTests[Atom.Id].uid)

  checkAll("Eq[Atom[GmosNorth]]", EqTests[Atom[GmosNorth]].eqv)
  checkAll("Atom.id",             LensTests(Atom.id[GmosNorth]))
  checkAll("Atom.steps",          LensTests(Atom.steps[GmosNorth]))

}

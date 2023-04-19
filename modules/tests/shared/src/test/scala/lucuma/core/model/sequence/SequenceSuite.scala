// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import lucuma.core.data.arb.ArbZipper
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


final class SequenceSuite extends DisciplineSuite {
  import ArbAtom.given
  import ArbBoundedCollection.given
  import ArbDynamicConfig.*
  import ArbEnumerated.*
  import ArbSequence.given
  import ArbUid.*

  override val scalaCheckTestParameters = Test.Parameters.default.withMaxSize(4)

  checkAll("Eq[Sequence[GmosNorth]]", EqTests[Sequence[GmosNorth]].eqv)
  checkAll("Sequence.atoms",          LensTests(Sequence.atoms[GmosNorth]))


}

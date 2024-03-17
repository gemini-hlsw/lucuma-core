// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos

import cats.kernel.laws.discipline.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.numeric.*
import lucuma.core.math.arb.ArbOffset
import lucuma.core.math.arb.ArbRefined
import lucuma.core.model.sequence.gmos.arb.*
import lucuma.core.util.arb.ArbEnumerated
import monocle.law.discipline.*
import munit.*

final class GmosNodAndShuffleSuite extends DisciplineSuite {
  import ArbEnumerated.given
  import ArbOffset.*
  import ArbGmosNodAndShuffle.*
  import ArbRefined.given

  checkAll("Eq[GmosNodAndShuffle]",           EqTests[GmosNodAndShuffle].eqv)
  checkAll("GmosNodAndShuffle.posA",          LensTests(GmosNodAndShuffle.posA))
  checkAll("GmosNodAndShuffle.posB",          LensTests(GmosNodAndShuffle.posB))
  checkAll("GmosNodAndShuffle.eOffset",       LensTests(GmosNodAndShuffle.eOffset))
  checkAll("GmosNodAndShuffle.shuffleOffset", LensTests(GmosNodAndShuffle.shuffleOffset))
  checkAll("GmosNodAndShuffle.shuffleCycles", LensTests(GmosNodAndShuffle.shuffleCycles))
}

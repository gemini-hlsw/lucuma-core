// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.kernel.laws.discipline._
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.numeric._
import lucuma.core.math.arb.ArbOffset
import lucuma.core.math.arb.ArbRefined
import lucuma.core.model.sequence.arb._
import lucuma.core.util.arb.ArbEnumerated
import monocle.law.discipline._
import munit._

final class GmosNodAndShuffleSuite extends DisciplineSuite {
  import ArbGmosNodAndShuffle._
  import ArbOffset._
  import ArbEnumerated._
  import ArbRefined._

  checkAll("Eq[GmosNodAndShuffle]", EqTests[GmosNodAndShuffle].eqv)
  checkAll("GmosNodAndShuffle.posA", LensTests(GmosNodAndShuffle.posA))
  checkAll("GmosNodAndShuffle.posB", LensTests(GmosNodAndShuffle.posB))
  checkAll("GmosNodAndShuffle.eOffset", LensTests(GmosNodAndShuffle.eOffset))
  checkAll("GmosNodAndShuffle.shuffleOffset", LensTests(GmosNodAndShuffle.shuffleOffset))
  checkAll("GmosNodAndShuffle.shuffleCycles", LensTests(GmosNodAndShuffle.shuffleCycles))
}

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.optics

import lucuma.core.optics.laws.discipline.*

final class WedgeSuite extends munit.DisciplineSuite {

  val se: SplitEpi[Long, Short] =
    SplitEpi(_.toShort, _.toLong)

  val sm: SplitMono[Short, Int] =
    SplitMono(_.toInt, _.toShort)

  val w: Wedge[Long, Int] =
    se.andThen(sm)

  // Laws
  checkAll("Long > Short", SplitEpiTests(se).splitEpi)
  checkAll("Short < Int", SplitMonoTests(sm).splitMono)
  checkAll("Long > Short < Int", WedgeTests(w).wedge)

}

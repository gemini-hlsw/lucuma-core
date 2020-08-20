// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math.skycalc.solver

import cats.tests.CatsSuite
import gsp.math.skycalc.solver.spireOptics._
import gsp.math.laws.discipline.SplitMonoTests
import spire.laws.arb._
import monocle.law.discipline.IsoTests

final class SpireOpticsSpec extends CatsSuite {
  checkAll("SplitMono[Int, Number]", SplitMonoTests(intNumber).splitMono)
  checkAll("Iso[Int, Number]", IsoTests(intNumberIso).iso)
}

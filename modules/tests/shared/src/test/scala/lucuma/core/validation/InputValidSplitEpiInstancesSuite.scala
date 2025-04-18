// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.validation

import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.all.*
import lucuma.core.math.arb.LimitedBigDecimals
import lucuma.core.optics.laws.discipline.ValidSplitEpiTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary

final class InputValidSplitEpiInstancesSuite extends DisciplineSuite with LimitedBigDecimals:
  // Laws
  checkAll("nonEmptyString", ValidSplitEpiTests(InputValidSplitEpi.nonEmptyString).validSplitEpi)
  checkAll("int", ValidSplitEpiTests(InputValidSplitEpi.int).validSplitEpi)
  checkAll("posInt", ValidSplitEpiTests(InputValidSplitEpi.posInt).validSplitEpi)
  checkAll("nonNegInt", ValidSplitEpiTests(InputValidSplitEpi.nonNegInt).validSplitEpi)
  checkAll("long", ValidSplitEpiTests(InputValidSplitEpi.long).validSplitEpi)
  checkAll("posLong", ValidSplitEpiTests(InputValidSplitEpi.posLong).validSplitEpi)
  checkAll("nonNegLong", ValidSplitEpiTests(InputValidSplitEpi.nonNegLong).validSplitEpi)
  checkAll("bigDecimal", ValidSplitEpiTests(InputValidSplitEpi.bigDecimal).validSplitEpi)
  checkAll("posBigDecimal", ValidSplitEpiTests(InputValidSplitEpi.posBigDecimal).validSplitEpi)
  checkAll("nonNegBigDecimal", ValidSplitEpiTests(InputValidSplitEpi.nonNegBigDecimal).validSplitEpi)
  checkAll(
    "bigDecimalWithScientificNotation",
    ValidSplitEpiTests(InputValidSplitEpi.bigDecimalWithScientificNotation).validSplitEpi
  )
  checkAll(
    "posBigDecimalWithScientificNotation",
    ValidSplitEpiTests(InputValidSplitEpi.posBigDecimalWithScientificNotation).validSplitEpi
  )

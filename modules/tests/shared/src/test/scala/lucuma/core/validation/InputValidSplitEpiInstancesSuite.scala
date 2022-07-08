// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.validation

import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.all._
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.optics.laws.discipline.ValidSplitEpiTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary

final class InputValidSplitEpiInstancesSuite extends DisciplineSuite {

  // The scientific notation formatters use `java.text.DecimalFormat` which in Scala.js seems
  // to have trouble formatting BigDecimals with very high absolute scale or precision.
  // We therefore use these bounded arbitraries.
  implicit lazy val arbBigDecimalLimitedPrecision: Arbitrary[BigDecimal] =
    Arbitrary(
      org.scalacheck.Arbitrary.arbBigDecimal.arbitrary.suchThat(x =>
        x.scale.abs < 100 && x.precision <= 15
      )
    )

  implicit lazy val arbPosBigDecimalLimitedPrecision: Arbitrary[PosBigDecimal] =
    Arbitrary(
      arbBigDecimalLimitedPrecision.arbitrary
        .map(_.abs)
        .suchThat(_ > 0)
        .map(PosBigDecimal.unsafeFrom)
    )

  // Laws
  checkAll("nonEmptyString", ValidSplitEpiTests(InputValidSplitEpi.nonEmptyString).validSplitEpi)
  checkAll("int", ValidSplitEpiTests(InputValidSplitEpi.int).validSplitEpi)
  checkAll("posInt", ValidSplitEpiTests(InputValidSplitEpi.posInt).validSplitEpi)
  checkAll("bigDecimal", ValidSplitEpiTests(InputValidSplitEpi.bigDecimal).validSplitEpi)
  checkAll("posBigDecimal", ValidSplitEpiTests(InputValidSplitEpi.posBigDecimal).validSplitEpi)
  checkAll(
    "bigDecimalWithScientificNotation",
    ValidSplitEpiTests(InputValidSplitEpi.bigDecimalWithScientificNotation).validSplitEpi
  )
  checkAll(
    "posBigDecimalWithScientificNotation",
    ValidSplitEpiTests(InputValidSplitEpi.posBigDecimalWithScientificNotation).validSplitEpi
  )
}

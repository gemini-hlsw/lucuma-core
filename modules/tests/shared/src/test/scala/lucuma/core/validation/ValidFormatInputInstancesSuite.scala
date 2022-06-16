// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.validation

import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.all._
import lucuma.core.math.truncated.arb._
import lucuma.core.optics.laws.discipline.ValidFormatTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import eu.timepit.refined.types.numeric.PosBigDecimal

final class ValidFormatInputInstancesSpec extends DisciplineSuite {
  import ArbTruncatedBigDecimal._
  import ArbTruncatedRefinedBigDecimal._

  // The scientific notation formatters use `java.text.DecimalFormat` which in Scala.js seems
  // to have trouble formatting BigDecimals with very high absolute scale or precision.
  // We therefore use these bounded arbitraries.
  implicit lazy val arbBigDecimalLimitedPrecision: Arbitrary[BigDecimal]       =
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
  checkAll("nonEmptyString", ValidFormatTests(ValidFormatInput.nonEmptyString).validFormat)
  checkAll("int", ValidFormatTests(ValidFormatInput.int).validFormat)
  checkAll("posInt", ValidFormatTests(ValidFormatInput.posInt).validFormat)
  checkAll("bigDecimal", ValidFormatTests(ValidFormatInput.bigDecimal).validFormat)
  checkAll("posBigDecimal", ValidFormatTests(ValidFormatInput.posBigDecimal).validFormat)
  checkAll(
    "truncatedBigDecimal[2]",
    ValidFormatTests(ValidFormatInput.truncatedBigDecimal[2]).validFormat
  )
  checkAll(
    "truncatedRefinedBigDecimal[OneToThree, 1]",
    ValidFormatTests(ValidFormatInput.truncatedRefinedBigDecimal[OneToThree, 1]).validFormat
  )

  checkAll(
    "bigDecimalWithScientificNotation",
    ValidFormatTests(ValidFormatInput.bigDecimalWithScientificNotation).validFormat
  )
  checkAll(
    "posBigDecimalWithScientificNotation",
    ValidFormatTests(ValidFormatInput.posBigDecimalWithScientificNotation).validFormat
  )
  // We want a truncatedbigdecimal with scientific notation
}

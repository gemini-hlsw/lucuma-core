// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.validation

import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.all._
import lucuma.core.math.truncated.arb._
import lucuma.core.optics.laws.discipline.ValidFormatTests
import munit.DisciplineSuite

final class ValidFormatInputInstancesSpec extends DisciplineSuite {
  import ArbTruncatedBigDecimal._
  import ArbTruncatedRefinedBigDecimal._

  // Laws
  checkAll(
    "nonEmptyString",
    ValidFormatTests(ValidFormatInput.nonEmptyString()).validFormat
  )

  checkAll("int", ValidFormatTests(ValidFormatInput.int()).validFormat)

  checkAll(
    "bigDecimal",
    ValidFormatTests(ValidFormatInput.bigDecimal()).validFormat
  )

  checkAll(
    "truncatedBigDecimal",
    ValidFormatTests(ValidFormatInput.truncatedBigDecimal[2]()).validFormat
  )

  checkAll(
    "truncatedRefinedBigDecimal",
    ValidFormatTests(ValidFormatInput.truncatedRefinedBigDecimal[OneToThree, 1]()).validFormat
  )
}

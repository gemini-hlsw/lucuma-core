// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.validation

import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import lucuma.core.math.arb.ArbRefined
import lucuma.core.optics.laws.discipline.ValidWedgeTests
import lucuma.refined._
import munit.DisciplineSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

final class InputValidWedgeInstancesSuite extends DisciplineSuite {
  import ArbRefined._

  val genNumericString: Gen[String]      = arbitrary[BigDecimal].map(_.toString)
  val genMaybeNumericString: Gen[String] =
    Gen.frequency(5 -> genNumericString, 1 -> arbitrary[String])

  checkAll(
    "truncatedBigDecimal(2)",
    ValidWedgeTests(InputValidWedge.truncatedBigDecimal(2.refined)).validWedgeWith(genMaybeNumericString)
  )

  checkAll(
    "truncatedPosBigDecimal(2)",
    ValidWedgeTests(InputValidWedge.truncatedPosBigDecimal(2.refined)).validWedgeWith(genMaybeNumericString)
  )

}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.validation

import cats.syntax.all._
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import eu.timepit.refined.scalacheck.all._
import lucuma.core.math.Epoch
import lucuma.core.math.arb.ArbEpoch._
import lucuma.core.validation.arb._
import lucuma.core.optics.laws.discipline.ValidFormatTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

final class ValidFormatInputInstancesSpec extends DisciplineSuite {
  import ArbTruncatedBigDecimal._
  import ArbTruncatedRefinedBigDecimal._
  import ArbTruncatedRA._
  import ArbTruncatedDec._

  val genUpperNES: Gen[UpperNES] =
    Gen.asciiStr.suchThat(_.nonEmpty).map(s => UpperNES.unsafeFrom(s.toUpperCase))

  implicit val arbUpperNES: Arbitrary[UpperNES] = Arbitrary(genUpperNES)

  // Laws
  checkAll("nonEmptyValidFormat",
           ValidFormatTests(ValidFormatInput.nonEmptyValidFormat).validFormat
  )
  checkAll("upperNESValidFormat",
           ValidFormatTests(ValidFormatInput.upperNESValidFormat).validFormat
  )
  checkAll("optionalEpochValidFormat",
           ValidFormatTests(ValidFormatInput.fromPrism(Epoch.fromString)).validFormat
  )
  checkAll("intValidFormat", ValidFormatTests(ValidFormatInput.intValidFormat()).validFormat)
  checkAll("bigDecimalValidFormat",
           ValidFormatTests(ValidFormatInput.bigDecimalValidFormat()).validFormat
  )
  checkAll("truncatedBigDecimalValidFormat",
           ValidFormatTests(ValidFormatInput.truncatedBigDecimalValidFormat[2]()).validFormat
  )
  checkAll(
    "forTruncatedRefinedBigDecimal",
    ValidFormatTests(ValidFormatInput.forRefinedTruncatedBigDecimal[OneToThree, 1]()).validFormat
  )
  checkAll("truncatedRAValidFormat", ValidFormatTests(ValidFormatInput.truncatedRA).validFormat)
  checkAll("truncatedDecValidFormat", ValidFormatTests(ValidFormatInput.truncatedDec).validFormat)
}

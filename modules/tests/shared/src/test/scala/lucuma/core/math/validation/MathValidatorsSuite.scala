// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.validation

import eu.timepit.refined.cats.*
import lucuma.core.math.Declination
import lucuma.core.math.HourAngle
import lucuma.core.math.arb.*
import lucuma.core.optics.laws.discipline.ValidSplitEpiTests
import lucuma.core.optics.laws.discipline.ValidWedgeTests
import munit.DisciplineSuite
import org.scalacheck.Arbitrary.*
import org.scalacheck.Gen

final class MathValidatorsSuite extends DisciplineSuite {
  import ArbAngle.given
  import ArbDeclination.given
  import ArbEpoch.given
  import ArbRightAscension.given

  val genNumericString      = arbitrary[BigDecimal].map(_.toString)
  val genMaybeNumericString = Gen.frequency(5 -> genNumericString, 1 -> arbitrary[String])

  checkAll("epoch", ValidSplitEpiTests(MathValidators.epoch).validSplitEpiLaws)
  checkAll("epochNoScheme", ValidWedgeTests(MathValidators.epochNoScheme).validWedgeLawsWith(ArbEpoch.stringsNoScheme))
  checkAll("angleArcSec", ValidSplitEpiTests(MathValidators.angleArcSec).validSplitEpiLaws)

  checkAll(
    "roundedAngleSignedArcSec",
    ValidWedgeTests(MathValidators.roundedAngleSignedArcSec).validWedgeWith(genNumericString)
  )

  checkAll(
    "roundedAngleDegrees",
    ValidWedgeTests(MathValidators.roundedAngleDegrees).validWedgeWith(genNumericString)
  )

  checkAll(
    "roundedAngleSignedDegrees",
    ValidWedgeTests(MathValidators.roundedAngleSignedDegrees).validWedgeWith(genNumericString)
  )

  val genHourAngleString      = arbitrary[HourAngle].map(HourAngle.fromStringHMS.reverseGet)
  val genMaybeHourAngleString = Gen.frequency(5 -> genHourAngleString, 1 -> arbitrary[String])

  checkAll(
    "rightAscension",
    ValidSplitEpiTests(MathValidators.rightAscension).validSplitEpiLawsWith(genMaybeHourAngleString)
  )

  checkAll(
    "roundedRA",
    ValidWedgeTests(MathValidators.roundedRA).validWedgeWith(genMaybeHourAngleString)
  )

  val genDeclinationString      = arbitrary[Declination].map(Declination.fromStringSignedDMS.reverseGet)
  val genMaybeDeclinationString = Gen.frequency(5 -> genDeclinationString, 1 -> arbitrary[String])

  // This could be stronger
  checkAll(
    "declination",
    ValidSplitEpiTests(MathValidators.declination).validSplitEpiLawsWith(genMaybeDeclinationString)
  )

  checkAll(
    "roundedDec",
    ValidWedgeTests(MathValidators.roundedDec).validWedgeWith(genMaybeDeclinationString)
  )
}

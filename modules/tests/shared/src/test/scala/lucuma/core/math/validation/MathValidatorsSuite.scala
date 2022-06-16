// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.validation

import eu.timepit.refined.cats._
import lucuma.core.math.arb._
import lucuma.core.math.truncated.arb._
import lucuma.core.optics.laws.discipline.ValidFormatTests
import munit.DisciplineSuite

final class MathValidatorsSuite extends DisciplineSuite {
  import ArbAngle._
  import ArbDeclination._
  import ArbEpoch._
  import ArbRightAscension._
  import ArbTruncatedAngle._
  import ArbTruncatedDec._
  import ArbTruncatedRA._

  checkAll("epoch", ValidFormatTests(MathValidators.epoch).validFormat)
  // checkAll("angle", ValidFormatTests(MathValidators.angle).validFormat)
  checkAll("truncatedAngle", ValidFormatTests(MathValidators.truncatedAngle).validFormat)
  checkAll("rightAscension", ValidFormatTests(MathValidators.rightAscension).validFormat)
  checkAll("truncatedRA", ValidFormatTests(MathValidators.truncatedRA).validFormat)
  checkAll("declination", ValidFormatTests(MathValidators.declination).validFormat)
  checkAll("truncatedDec", ValidFormatTests(MathValidators.truncatedDec).validFormat)
}

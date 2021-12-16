// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
import cats.kernel.laws.discipline._
import lucuma.core.model.arb._
import munit._
import lucuma.core.math.BrightnessUnits
import lucuma.core.util.arb.ArbEnumerated
import monocle.law.discipline.LensTests
import lucuma.core.math.arb.ArbBrightnessValue
import lucuma.core.math.dimensional.arb.ArbQty

final class TargetBrightnessSuite extends DisciplineSuite {
  import ArbTargetBrightness._
  import ArbEnumerated._
  import BrightnessUnits._
  import ArbBrightnessValue._
  import ArbQty._

  // Laws
  checkAll(
    "Order[TargetBrightness[Integrated]]",
    OrderTests[TargetBrightness[Integrated]].order
  )
  checkAll(
    "Order[TargetBrightness[Surface]]",
    OrderTests[TargetBrightness[Surface]].order
  )

  checkAll(
    "TargetBrightness.quantity[Integrated]",
    LensTests(TargetBrightness.quantity[Integrated])
  )
  checkAll(
    "TargetBrightness.value[Integrated]",
    LensTests(TargetBrightness.value[Integrated])
  )
  checkAll(
    "TargetBrightness.unit[Integrated]",
    LensTests(TargetBrightness.unit[Integrated])
  )
  checkAll(
    "TargetBrightness.band[Integrated]",
    LensTests(TargetBrightness.band[Integrated])
  )
  checkAll(
    "TargetBrightness.error[Integrated]",
    LensTests(TargetBrightness.error[Integrated])
  )
}

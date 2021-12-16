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

final class BandBrightnessSuite extends DisciplineSuite {
  import ArbBandBrightness._
  import ArbEnumerated._
  import BrightnessUnits._
  import ArbBrightnessValue._
  import ArbQty._

  // Laws
  checkAll(
    "Order[BandBrightness[Integrated]]",
    OrderTests[BandBrightness[Integrated]].order
  )
  checkAll(
    "Order[BandBrightness[Surface]]",
    OrderTests[BandBrightness[Surface]].order
  )

  checkAll(
    "BandBrightness.quantity[Integrated]",
    LensTests(BandBrightness.quantity[Integrated])
  )
  checkAll(
    "BandBrightness.quantity[Surface]",
    LensTests(BandBrightness.quantity[Surface])
  )
  checkAll(
    "BandBrightness.value[Integrated]",
    LensTests(BandBrightness.value[Integrated])
  )
  checkAll(
    "BandBrightness.value[Surface]",
    LensTests(BandBrightness.value[Surface])
  )
  checkAll(
    "BandBrightness.unit[Integrated]",
    LensTests(BandBrightness.unit[Integrated])
  )
  checkAll(
    "BandBrightness.unit[Surface]",
    LensTests(BandBrightness.unit[Surface])
  )
  checkAll(
    "BandBrightness.band[Integrated]",
    LensTests(BandBrightness.band[Integrated])
  )
  checkAll(
    "BandBrightness.band[Surface]",
    LensTests(BandBrightness.band[Surface])
  )
  checkAll(
    "BandBrightness.error[Integrated]",
    LensTests(BandBrightness.error[Integrated])
  )
  checkAll(
    "BandBrightness.error[Surface]",
    LensTests(BandBrightness.error[Surface])
  )
}

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import cats.syntax.all._
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnits
import lucuma.core.math.BrightnessValue
import lucuma.core.math.arb.ArbBrightnessValue
import lucuma.core.math.dimensional._
import lucuma.core.math.dimensional.arb.ArbQty
import lucuma.core.math.units._
import lucuma.core.model.arb._
import lucuma.core.util.arb.ArbEnumerated
import monocle.law.discipline.LensTests
import munit._

final class BandBrightnessSuite extends DisciplineSuite {
  import ArbBandBrightness._
  import ArbEnumerated._
  import BrightnessUnits._
  import ArbBrightnessValue._
  import ArbQty._

  def checkValues[T, U](
    b:           Option[BandBrightness[T]]
  )(scaledValue: Int, band: Band, scaledError: Option[Int])(implicit
    unit:        UnitOfMeasure[U]
  ): Unit = {
    assertEquals(b.map(_.quantity.value.scaledValue), scaledValue.some)
    assertEquals(b.map(_.quantity.unit.ungrouped), unit.some)
    assertEquals(b.map(_.band), band.some)
    assertEquals(b.flatMap(_.error.map(_.scaledValue)), scaledError)
  }

  // Full constructors
  val b1 = BandBrightness(
    GroupedUnitOfMeasure[Brightness[Integrated], VegaMagnitude]
      .withValue(BrightnessValue.fromDouble(10.0)),
    Band.R
  )
  checkValues[Integrated, VegaMagnitude](b1.some)(10000, Band.R, None)

  val b2 = BandBrightness(
    GroupedUnitOfMeasure[Brightness[Integrated], VegaMagnitude]
      .withValue(BrightnessValue.fromDouble(10.0)),
    Band.R,
    BrightnessValue.fromDouble(2.0)
  )
  checkValues[Integrated, VegaMagnitude](b2.some)(10000, Band.R, 2000.some)

  // Convenience constructors
  val b3 = BandBrightness[Surface, ABMagnitudePerArcsec2](BrightnessValue.fromDouble(10.0), Band.R)
  checkValues[Surface, ABMagnitudePerArcsec2](b3)(10000, Band.R, None)

  val b4 = BandBrightness[Surface, ABMagnitudePerArcsec2](
    BrightnessValue.fromDouble(10.0),
    Band.R,
    BrightnessValue.fromDouble(2.0)
  )
  checkValues[Surface, ABMagnitudePerArcsec2](b4)(10000, Band.R, 2000.some)

  // Default units
  val b5 = BandBrightness[Integrated](BrightnessValue.fromDouble(10.0), Band.R)
  checkValues[Integrated, VegaMagnitude](b5.some)(10000, Band.R, None)

  val b6 = BandBrightness[Integrated](
    BrightnessValue.fromDouble(10.0),
    Band.R,
    BrightnessValue.fromDouble(2.0)
  )
  checkValues[Integrated, VegaMagnitude](b6.some)(10000, Band.R, 2000.some)

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

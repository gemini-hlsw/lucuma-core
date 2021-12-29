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
import lucuma.core.math.dimensional.arb.ArbMeasure
import lucuma.core.math.units._
import lucuma.core.model.arb._
import lucuma.core.util.arb.ArbEnumerated
import monocle.law.discipline.LensTests
import munit._
import coulomb._

final class BandBrightnessSuite extends DisciplineSuite {
  import ArbBandBrightness._
  import ArbEnumerated._
  import BrightnessUnits._
  import ArbBrightnessValue._
  import ArbMeasure._

  def checkValues[T, U](name: String)(
    b:                        BandBrightness[T]
  )(scaledValue:              Int, band: Band, scaledError: Option[Int])(implicit
    units:                    UnitOfMeasure[U]
  ): Unit = test(s"Constructor: $name") {
    assertEquals(b.quantity.value.scaledValue, scaledValue)
    assertEquals(b.quantity.units, units)
    assertEquals(b.band, band)
    assertEquals(b.error.map(_.scaledValue), scaledError)
  }

  // Full constructors
  val b1 = BandBrightness(
    BrightnessValue
      .fromDouble(10.0)
      .withUnit[VegaMagnitude]
      .toMeasureTagged[Brightness[Integrated]],
    Band.R
  )
  checkValues[Integrated, VegaMagnitude]("Full without error")(b1)(10000, Band.R, None)

  val b2 = BandBrightness(
    BrightnessValue
      .fromDouble(10.0)
      .withUnit[VegaMagnitude]
      .toMeasureTagged[Brightness[Integrated]],
    Band.R,
    BrightnessValue.fromDouble(2.0)
  )
  checkValues[Integrated, VegaMagnitude]("Full with error")(b2)(
    10000,
    Band.R,
    2000.some
  )

  // Convenience constructors
  val b3 = BandBrightness[Surface, ABMagnitudePerArcsec2](BrightnessValue.fromDouble(10.0), Band.R)
  checkValues[Surface, ABMagnitudePerArcsec2](
    "Type-parametrized units without error"
  )(b3)(10000, Band.R, None)

  val b4 = BandBrightness[Surface, ABMagnitudePerArcsec2](
    BrightnessValue.fromDouble(10.0),
    Band.R,
    BrightnessValue.fromDouble(2.0)
  )
  checkValues[Surface, ABMagnitudePerArcsec2]("Type-parametrized units with error")(b4)(
    10000,
    Band.R,
    2000.some
  )

  // Default units
  val b5 = BandBrightness[Integrated](BrightnessValue.fromDouble(10.0), Band.R)
  checkValues[Integrated, VegaMagnitude]("Default band units without error")(b5)(
    10000,
    Band.R,
    None
  )

  val b6 = BandBrightness[Surface](
    BrightnessValue.fromDouble(10.0),
    Band.SloanR,
    BrightnessValue.fromDouble(2.0)
  )
  checkValues[Surface, ABMagnitudePerArcsec2]("Default band units with error")(b6)(
    10000,
    Band.SloanR,
    2000.some
  )

  // Brightness type conversions
  val b1Surface = BandBrightness(
    BrightnessValue
      .fromDouble(10.0)
      .withUnit[VegaMagnitudePerArcsec2]
      .toMeasureTagged[Brightness[Surface]],
    Band.R
  )
  test("Brightness type conversion Integrated -> Surface") {
    assertEquals(b1.to[Surface], b1Surface)
  }

  test("Brightness identity type conversion") {
    assertEquals(b1Surface.to[Surface], b1Surface)
  }

  test("Brightness type conversion roundtrip") {
    assertEquals(b1Surface.to[Integrated].to[Surface], b1Surface)
  }

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
    LensTests(BandBrightness.units[Integrated])
  )
  checkAll(
    "BandBrightness.unit[Surface]",
    LensTests(BandBrightness.units[Surface])
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

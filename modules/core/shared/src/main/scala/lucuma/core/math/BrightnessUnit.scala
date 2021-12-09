// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package math

import lucuma.core.math.dimensional._
import lucuma.core.math.units._
import lucuma.core.util.Enumerated

object BrightnessUnit {
  type Integrated
  type Surface

  object Integrated {
    val all: List[GroupedUnitType[Integrated]] =
      List(
        UnitOfMeasure[VegaMagnitude],
        UnitOfMeasure[ABMagnitude],
        UnitOfMeasure[Jansky],
        UnitOfMeasure[WattsBrightness],
        UnitOfMeasure[ErgsWavelengthBrightness],
        UnitOfMeasure[ErgsFrequencyBrightness]
      ).map(_.groupedIn[Integrated])
  }

  object Surface {
    val all: List[GroupedUnitType[Surface]] =
      List(
        UnitOfMeasure[VegaMagnitudePerArcsec2],
        UnitOfMeasure[ABMagnitudePerArcsec2],
        UnitOfMeasure[JanskyPerArcsec2],
        UnitOfMeasure[WattsBrightnessPerArcsec2],
        UnitOfMeasure[ErgsWavelengthBrightnessPerArcsec2],
        UnitOfMeasure[ErgsFrequencyBrightnessPerArcsec2]
      ).map(_.groupedIn[Surface])
  }

  implicit val enumDimUnitTypeIntegrated: Enumerated[GroupedUnitType[Integrated]] =
    new Enumerated[GroupedUnitType[Integrated]] {
      val all                                                            = Integrated.all
      def tag(a: GroupedUnitType[Integrated])                            = a.definition.abbv
      override def unsafeFromTag(s: String): GroupedUnitType[Integrated] =
        all.find(_.definition.abbv == s).get
    }

  implicit val enumDimUnitTypeSurface: Enumerated[GroupedUnitType[Surface]] =
    new Enumerated[GroupedUnitType[Surface]] {
      val all                                                         = Surface.all
      def tag(a: GroupedUnitType[Surface])                            = a.definition.abbv
      override def unsafeFromTag(s: String): GroupedUnitType[Surface] =
        all.find(_.definition.abbv == s).get
    }
}

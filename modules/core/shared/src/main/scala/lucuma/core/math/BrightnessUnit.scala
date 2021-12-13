// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package math

import lucuma.core.math.dimensional._
import lucuma.core.math.units._
import lucuma.core.util.Enumerated
import cats.syntax.all._

object BrightnessUnit {
  type Integrated
  type Surface

  trait Brightness[+T]
  trait LineFlux[+T]
  trait ContinuumFluxDensity[+T]

  object Integrated {
    val all: List[GroupedUnitType[Brightness[Integrated]]] =
      List(
        UnitOfMeasure[VegaMagnitude],
        UnitOfMeasure[ABMagnitude],
        UnitOfMeasure[Jansky],
        UnitOfMeasure[WattsBrightness],
        UnitOfMeasure[ErgsWavelengthBrightness],
        UnitOfMeasure[ErgsFrequencyBrightness]
      ).map(_.groupedIn[Brightness[Integrated]])
  }

  object Surface {
    val all: List[GroupedUnitType[Brightness[Surface]]] =
      List(
        UnitOfMeasure[VegaMagnitudePerArcsec2],
        UnitOfMeasure[ABMagnitudePerArcsec2],
        UnitOfMeasure[JanskyPerArcsec2],
        UnitOfMeasure[WattsBrightnessPerArcsec2],
        UnitOfMeasure[ErgsWavelengthBrightnessPerArcsec2],
        UnitOfMeasure[ErgsFrequencyBrightnessPerArcsec2]
      ).map(_.groupedIn[Brightness[Surface]])
  }

  implicit val enumDimUnitTypeIntegrated: Enumerated[GroupedUnitType[Brightness[Integrated]]] =
    new Enumerated[GroupedUnitType[Brightness[Integrated]]] {
      val all                                                                        = Integrated.all
      def tag(a: GroupedUnitType[Brightness[Integrated]])                            = a.definition.abbv
      override def unsafeFromTag(s: String): GroupedUnitType[Brightness[Integrated]] =
        all.find(tag(_) === s).get
    }

  implicit val enumDimUnitTypeSurface: Enumerated[GroupedUnitType[Brightness[Surface]]] =
    new Enumerated[GroupedUnitType[Brightness[Surface]]] {
      val all                                                                     = Surface.all
      def tag(a: GroupedUnitType[Brightness[Surface]])                            = a.definition.abbv
      override def unsafeFromTag(s: String): GroupedUnitType[Brightness[Surface]] =
        all.find(tag(_) === s).get
    }
}

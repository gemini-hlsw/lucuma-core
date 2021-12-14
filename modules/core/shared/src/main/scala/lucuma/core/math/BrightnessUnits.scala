// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package math

import cats.syntax.all._
import lucuma.core.math.dimensional._
import lucuma.core.math.units._
import lucuma.core.util.Enumerated

object BrightnessUnits {
  type Integrated
  type Surface

  trait Brightness[+T]
  trait LineFlux[+T]
  trait ContinuumFluxDensity[+T]

  object Brightness {
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
  }

  object LineFlux {
    object Integrated {
      val all: List[GroupedUnitType[LineFlux[Integrated]]] =
        List(
          UnitOfMeasure[WattsLineFlux],
          UnitOfMeasure[ErgsLineFlux]
        ).map(_.groupedIn[LineFlux[Integrated]])
    }

    object Surface {
      val all: List[GroupedUnitType[LineFlux[Surface]]] =
        List(
          UnitOfMeasure[WattsLineFluxPerArcsec2],
          UnitOfMeasure[ErgsLineFluxPerArcsec2]
        ).map(_.groupedIn[LineFlux[Surface]])
    }
  }

  object ContinuumFluxDensity {
    object Integrated {
      val all: List[GroupedUnitType[ContinuumFluxDensity[Integrated]]] =
        List(
          UnitOfMeasure[WattsBrightness],
          UnitOfMeasure[ErgsWavelengthBrightness]
        ).map(_.groupedIn[ContinuumFluxDensity[Integrated]])
    }

    object Surface {
      val all: List[GroupedUnitType[ContinuumFluxDensity[Surface]]] =
        List(
          UnitOfMeasure[WattsBrightnessPerArcsec2],
          UnitOfMeasure[ErgsWavelengthBrightnessPerArcsec2]
        ).map(_.groupedIn[ContinuumFluxDensity[Surface]])
    }

  }

  private def enumGroupedUnitType[UG](
    allList: List[GroupedUnitType[UG]]
  ): Enumerated[GroupedUnitType[UG]] = new Enumerated[GroupedUnitType[UG]] {
    val all                                                    = allList
    def tag(a: GroupedUnitType[UG])                            = a.definition.abbv
    override def unsafeFromTag(s: String): GroupedUnitType[UG] = all.find(tag(_) === s).get
  }

  implicit val enumBrightnessIntegrated: Enumerated[GroupedUnitType[Brightness[Integrated]]] =
    enumGroupedUnitType[Brightness[Integrated]](Brightness.Integrated.all)

  implicit val enumBrightnessSurface: Enumerated[GroupedUnitType[Brightness[Surface]]] =
    enumGroupedUnitType[Brightness[Surface]](Brightness.Surface.all)

  implicit val enumLineFluxIntegrated: Enumerated[GroupedUnitType[LineFlux[Integrated]]] =
    enumGroupedUnitType[LineFlux[Integrated]](LineFlux.Integrated.all)

  implicit val enumLineFluxSurface: Enumerated[GroupedUnitType[LineFlux[Surface]]] =
    enumGroupedUnitType[LineFlux[Surface]](LineFlux.Surface.all)

  implicit val enumContinuumFluxDensityIntegrated
    : Enumerated[GroupedUnitType[ContinuumFluxDensity[Integrated]]] =
    enumGroupedUnitType[ContinuumFluxDensity[Integrated]](ContinuumFluxDensity.Integrated.all)

  implicit val enumContinuumFluxDensitySurface
    : Enumerated[GroupedUnitType[ContinuumFluxDensity[Surface]]] =
    enumGroupedUnitType[ContinuumFluxDensity[Surface]](ContinuumFluxDensity.Surface.all)
}

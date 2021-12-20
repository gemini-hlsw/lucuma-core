// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package math

import cats.data.NonEmptyList
import lucuma.core.math.dimensional._
import lucuma.core.math.units._
import lucuma.core.util.Enumerated
// import coulomb._
// import coulomb.accepted._
// import coulomb.define._
// import coulomb.mks._
// import coulomb.si._
// import coulomb.siprefix._
// import coulomb.time._

object BrightnessUnits {
  type Integrated
  type Surface

  trait Brightness[+T]
  trait LineFlux[+T]
  trait FluxDensityContinuum[+T]

  object Brightness {
    object Integrated {
      val all: NonEmptyList[GroupedUnitType[Brightness[Integrated]]] =
        NonEmptyList
          .of(
            UnitOfMeasure[VegaMagnitude],
            UnitOfMeasure[ABMagnitude],
            UnitOfMeasure[Jansky],
            UnitOfMeasure[WattsBrightness],
            UnitOfMeasure[ErgsWavelengthBrightness],
            UnitOfMeasure[ErgsFrequencyBrightness]
          )
          .map(_.groupedIn[Brightness[Integrated]])
    }

    object Surface {
      val all: NonEmptyList[GroupedUnitType[Brightness[Surface]]] =
        NonEmptyList
          .of(
            UnitOfMeasure[VegaMagnitudePerArcsec2],
            UnitOfMeasure[ABMagnitudePerArcsec2],
            UnitOfMeasure[JanskyPerArcsec2],
            UnitOfMeasure[SurfaceUnits[WattsBrightness]],
            UnitOfMeasure[ErgsWavelengthBrightnessPerArcsec2],
            UnitOfMeasure[ErgsFrequencyBrightnessPerArcsec2]
          )
          .map(_.groupedIn[Brightness[Surface]])
    }
  }

  object LineFlux {
    object Integrated {
      val all: NonEmptyList[GroupedUnitType[LineFlux[Integrated]]] =
        NonEmptyList
          .of(
            UnitOfMeasure[WattsLineFlux],
            UnitOfMeasure[ErgsLineFlux]
          )
          .map(_.groupedIn[LineFlux[Integrated]])
    }

    object Surface {
      val all: NonEmptyList[GroupedUnitType[LineFlux[Surface]]] =
        NonEmptyList
          .of(
            UnitOfMeasure[WattsLineFluxPerArcsec2],
            UnitOfMeasure[ErgsLineFluxPerArcsec2]
          )
          .map(_.groupedIn[LineFlux[Surface]])
    }
  }

  object FluxDensityContinuum {
    object Integrated {
      val all: NonEmptyList[GroupedUnitType[FluxDensityContinuum[Integrated]]] =
        NonEmptyList
          .of(
            UnitOfMeasure[WattsBrightness],
            UnitOfMeasure[ErgsWavelengthBrightness]
          )
          .map(_.groupedIn[FluxDensityContinuum[Integrated]])
    }

    object Surface {
      val all: NonEmptyList[GroupedUnitType[FluxDensityContinuum[Surface]]] =
        NonEmptyList
          .of(
            UnitOfMeasure[SurfaceUnits[WattsBrightness]],
            UnitOfMeasure[ErgsWavelengthBrightnessPerArcsec2]
          )
          .map(_.groupedIn[FluxDensityContinuum[Surface]])
    }

  }

  private def enumGroupedUnitType[UG](
    allList: NonEmptyList[GroupedUnitType[UG]]
  ): Enumerated[GroupedUnitType[UG]] =
    Enumerated.fromNEL(allList).withTag(_.abbv)

  implicit val enumBrightnessIntegrated: Enumerated[GroupedUnitType[Brightness[Integrated]]] =
    enumGroupedUnitType[Brightness[Integrated]](Brightness.Integrated.all)

  implicit val enumBrightnessSurface: Enumerated[GroupedUnitType[Brightness[Surface]]] =
    enumGroupedUnitType[Brightness[Surface]](Brightness.Surface.all)

  implicit val enumLineFluxIntegrated: Enumerated[GroupedUnitType[LineFlux[Integrated]]] =
    enumGroupedUnitType[LineFlux[Integrated]](LineFlux.Integrated.all)

  implicit val enumLineFluxSurface: Enumerated[GroupedUnitType[LineFlux[Surface]]] =
    enumGroupedUnitType[LineFlux[Surface]](LineFlux.Surface.all)

  implicit val enumFluxDensityContinuumIntegrated
    : Enumerated[GroupedUnitType[FluxDensityContinuum[Integrated]]] =
    enumGroupedUnitType[FluxDensityContinuum[Integrated]](FluxDensityContinuum.Integrated.all)

  implicit val enumFluxDensityContinuumSurface
    : Enumerated[GroupedUnitType[FluxDensityContinuum[Surface]]] =
    enumGroupedUnitType[FluxDensityContinuum[Surface]](FluxDensityContinuum.Surface.all)
}

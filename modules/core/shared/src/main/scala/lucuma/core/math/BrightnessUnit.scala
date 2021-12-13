// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package math

import cats.syntax.all._
import lucuma.core.math.dimensional._
import lucuma.core.math.units._
import lucuma.core.util.Enumerated

object BrightnessUnit {
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

  implicit val enumBrightnessIntegrated: Enumerated[GroupedUnitType[Brightness[Integrated]]] =
    new Enumerated[GroupedUnitType[Brightness[Integrated]]] {
      val all                                                                        = Brightness.Integrated.all
      def tag(a: GroupedUnitType[Brightness[Integrated]])                            = a.definition.abbv
      override def unsafeFromTag(s: String): GroupedUnitType[Brightness[Integrated]] =
        all.find(tag(_) === s).get
    }

  implicit val enumBrightnessSurface: Enumerated[GroupedUnitType[Brightness[Surface]]] =
    new Enumerated[GroupedUnitType[Brightness[Surface]]] {
      val all                                                                     = Brightness.Surface.all
      def tag(a: GroupedUnitType[Brightness[Surface]])                            = a.definition.abbv
      override def unsafeFromTag(s: String): GroupedUnitType[Brightness[Surface]] =
        all.find(tag(_) === s).get
    }

  implicit val enumLineFluxIntegrated: Enumerated[GroupedUnitType[LineFlux[Integrated]]] =
    new Enumerated[GroupedUnitType[LineFlux[Integrated]]] {
      val all                                                                      = LineFlux.Integrated.all
      def tag(a: GroupedUnitType[LineFlux[Integrated]])                            = a.definition.abbv
      override def unsafeFromTag(s: String): GroupedUnitType[LineFlux[Integrated]] =
        all.find(tag(_) === s).get
    }

  implicit val enumLineFluxSurface: Enumerated[GroupedUnitType[LineFlux[Surface]]] =
    new Enumerated[GroupedUnitType[LineFlux[Surface]]] {
      val all                                                                   = LineFlux.Surface.all
      def tag(a: GroupedUnitType[LineFlux[Surface]])                            = a.definition.abbv
      override def unsafeFromTag(s: String): GroupedUnitType[LineFlux[Surface]] =
        all.find(tag(_) === s).get
    }

  //

  implicit val enumContinuumFluxDensityIntegrated
    : Enumerated[GroupedUnitType[ContinuumFluxDensity[Integrated]]] =
    new Enumerated[GroupedUnitType[ContinuumFluxDensity[Integrated]]] {
      val all                                                                                  = ContinuumFluxDensity.Integrated.all
      def tag(a: GroupedUnitType[ContinuumFluxDensity[Integrated]])                            = a.definition.abbv
      override def unsafeFromTag(s: String): GroupedUnitType[ContinuumFluxDensity[Integrated]] =
        all.find(tag(_) === s).get
    }

  implicit val enumContinuumFluxDensitySurface
    : Enumerated[GroupedUnitType[ContinuumFluxDensity[Surface]]] =
    new Enumerated[GroupedUnitType[ContinuumFluxDensity[Surface]]] {
      val all                                                                               = ContinuumFluxDensity.Surface.all
      def tag(a: GroupedUnitType[ContinuumFluxDensity[Surface]])                            = a.definition.abbv
      override def unsafeFromTag(s: String): GroupedUnitType[ContinuumFluxDensity[Surface]] =
        all.find(tag(_) === s).get
    }
}

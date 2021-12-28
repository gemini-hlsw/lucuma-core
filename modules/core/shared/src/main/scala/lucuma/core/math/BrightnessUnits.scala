// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package math

import cats.data.NonEmptyList
import lucuma.core.math.dimensional._
import lucuma.core.math.units._
import lucuma.core.util.Enumerated
import shapeless.tag.@@

object BrightnessUnits {
  type Integrated
  type Surface

  trait Brightness[+T]
  trait LineFlux[+T]
  trait FluxDensityContinuum[+T]

  // Brightness Integrated
  implicit object VegaMagnitudeIsIntegratedBrightnessUnit
      extends IsTaggedUnit[VegaMagnitude, Brightness[Integrated]]
  implicit object ABMagnitudeIsIntegratedBrightnessUnit
      extends IsTaggedUnit[ABMagnitude, Brightness[Integrated]]
  implicit object JanskyIsIntegratedBrightnessUnit
      extends IsTaggedUnit[Jansky, Brightness[Integrated]]
  implicit object WattsPerMeter2MicrometerIsIntegratedBrightnessUnit
      extends IsTaggedUnit[WattsPerMeter2Micrometer, Brightness[Integrated]]
  implicit object ErgsPerSecondCentimeter2AngstromIsIntegratedBrightnessUnit
      extends IsTaggedUnit[ErgsPerSecondCentimeter2Angstrom, Brightness[Integrated]]
  implicit object ErgsPerSecondCentimeter2HertzIsIntegratedBrightnessUnit
      extends IsTaggedUnit[ErgsPerSecondCentimeter2Hertz, Brightness[Integrated]]

  // Brightness Surface
  implicit object VegaMagnitudePerArcsec2IsSurfaceBrightnessUnit
      extends IsTaggedUnit[VegaMagnitudePerArcsec2, Brightness[Surface]]
  implicit object ABMagnitudePerArcsec2IsSurfaceBrightnessUnit
      extends IsTaggedUnit[ABMagnitudePerArcsec2, Brightness[Surface]]
  implicit object JanskyPerArcsec2IsSurfaceBrightnessUnit
      extends IsTaggedUnit[JanskyPerArcsec2, Brightness[Surface]]
  implicit object WattsPerMeter2MicrometerArcsec2IsSurfaceBrightnessUnit
      extends IsTaggedUnit[WattsPerMeter2MicrometerArcsec2, Brightness[Surface]]
  implicit object ErgsPerSecondCentimeter2AngstromArcsec2IsSurfaceBrightnessUnit
      extends IsTaggedUnit[ErgsPerSecondCentimeter2AngstromArcsec2, Brightness[Surface]]
  implicit object ErgsPerSecondCentimeter2HertzArcsec2IsSurfaceBrightnessUnit
      extends IsTaggedUnit[ErgsPerSecondCentimeter2HertzArcsec2, Brightness[Surface]]

  // Line Flux Integrated
  implicit object WattsPerMeter2IsIntegratedLineFluxUnit
      extends IsTaggedUnit[WattsPerMeter2, LineFlux[Integrated]]
  implicit object ErgsPerSecondCentimeter2IsIntegratedLineFluxUnit
      extends IsTaggedUnit[ErgsPerSecondCentimeter2, LineFlux[Integrated]]

  // Line Flux Surface
  implicit object WattsPerMeter2Arcsec2IsSurfaceLineFluxUnit
      extends IsTaggedUnit[WattsPerMeter2Arcsec2, LineFlux[Surface]]
  implicit object ErgsPerSecondCentimeter2Arcsec2IsSurfaceLineFluxUnit
      extends IsTaggedUnit[ErgsPerSecondCentimeter2Arcsec2, LineFlux[Surface]]

  // Flux Density Continuum Integrated
  implicit object WattsPerMeter2MicrometerIsIntegratedFluxDensityContinuumUnit
      extends IsTaggedUnit[WattsPerMeter2Micrometer, FluxDensityContinuum[Integrated]]
  implicit object ErgsPerSecondCentimeter2AngstromIsIntegratedFluxDensityContinuumUnit
      extends IsTaggedUnit[ErgsPerSecondCentimeter2Angstrom, FluxDensityContinuum[Integrated]]

  // Flux Density Continuum Surface
  implicit object WattsPerMeter2MicrometerArcsec2IsSurfaceFluxDensityContinuumUnit
      extends IsTaggedUnit[WattsPerMeter2MicrometerArcsec2, FluxDensityContinuum[Surface]]
  implicit object ErgsPerSecondCentimeter2AngstromArcsec2IsSurfaceFluxDensityContinuumUnit
      extends IsTaggedUnit[ErgsPerSecondCentimeter2AngstromArcsec2, FluxDensityContinuum[Surface]]

  object Brightness {
    object Integrated {

      val all: NonEmptyList[UnitType @@ Brightness[Integrated]] =
        NonEmptyList
          .of(
            VegaMagnitudeIsIntegratedBrightnessUnit,
            ABMagnitudeIsIntegratedBrightnessUnit,
            JanskyIsIntegratedBrightnessUnit,
            WattsPerMeter2MicrometerIsIntegratedBrightnessUnit,
            ErgsPerSecondCentimeter2AngstromIsIntegratedBrightnessUnit,
            ErgsPerSecondCentimeter2HertzIsIntegratedBrightnessUnit
          )
          .map(_.unit)
    }

    object Surface {

      val all: NonEmptyList[UnitType @@ Brightness[Surface]] =
        NonEmptyList
          .of(
            VegaMagnitudePerArcsec2IsSurfaceBrightnessUnit,
            ABMagnitudePerArcsec2IsSurfaceBrightnessUnit,
            JanskyPerArcsec2IsSurfaceBrightnessUnit,
            WattsPerMeter2MicrometerArcsec2IsSurfaceBrightnessUnit,
            ErgsPerSecondCentimeter2AngstromArcsec2IsSurfaceBrightnessUnit,
            ErgsPerSecondCentimeter2HertzArcsec2IsSurfaceBrightnessUnit
          )
          .map(_.unit)
    }
  }

  object LineFlux {
    object Integrated {
      val all: NonEmptyList[UnitType @@ LineFlux[Integrated]] =
        NonEmptyList
          .of(
            WattsPerMeter2IsIntegratedLineFluxUnit,
            ErgsPerSecondCentimeter2IsIntegratedLineFluxUnit
          )
          .map(_.unit)
    }

    object Surface {
      val all: NonEmptyList[UnitType @@ LineFlux[Surface]] =
        NonEmptyList
          .of(
            WattsPerMeter2Arcsec2IsSurfaceLineFluxUnit,
            ErgsPerSecondCentimeter2Arcsec2IsSurfaceLineFluxUnit
          )
          .map(_.unit)
    }
  }

  object FluxDensityContinuum {
    object Integrated {
      val all: NonEmptyList[UnitType @@ FluxDensityContinuum[Integrated]] =
        NonEmptyList
          .of(
            WattsPerMeter2MicrometerIsIntegratedFluxDensityContinuumUnit,
            ErgsPerSecondCentimeter2AngstromIsIntegratedFluxDensityContinuumUnit
          )
          .map(_.unit)
    }

    object Surface {
      val all: NonEmptyList[UnitType @@ FluxDensityContinuum[Surface]] =
        NonEmptyList
          .of(
            WattsPerMeter2MicrometerArcsec2IsSurfaceFluxDensityContinuumUnit,
            ErgsPerSecondCentimeter2AngstromArcsec2IsSurfaceFluxDensityContinuumUnit
          )
          .map(_.unit)
    }

  }

  // Enumerated instances

  private def enumTaggedUnit[Tag](
    allList: NonEmptyList[UnitType @@ Tag]
  ): Enumerated[UnitType @@ Tag] =
    Enumerated.fromNEL(allList).withTag(_.abbv)

  implicit val enumBrightnessIntegrated: Enumerated[UnitType @@ Brightness[Integrated]] =
    enumTaggedUnit[Brightness[Integrated]](Brightness.Integrated.all)

  implicit val enumBrightnessSurface: Enumerated[UnitType @@ Brightness[Surface]] =
    enumTaggedUnit[Brightness[Surface]](Brightness.Surface.all)

  implicit val enumLineFluxIntegrated: Enumerated[UnitType @@ LineFlux[Integrated]] =
    enumTaggedUnit[LineFlux[Integrated]](LineFlux.Integrated.all)

  implicit val enumLineFluxSurface: Enumerated[UnitType @@ LineFlux[Surface]] =
    enumTaggedUnit[LineFlux[Surface]](LineFlux.Surface.all)

  implicit val enumFluxDensityContinuumIntegrated
    : Enumerated[UnitType @@ FluxDensityContinuum[Integrated]] =
    enumTaggedUnit[FluxDensityContinuum[Integrated]](FluxDensityContinuum.Integrated.all)

  implicit val enumFluxDensityContinuumSurface
    : Enumerated[UnitType @@ FluxDensityContinuum[Surface]] =
    enumTaggedUnit[FluxDensityContinuum[Surface]](FluxDensityContinuum.Surface.all)

  /**
   * Type class providing a conversion between a `UnitType` tagged with `T` to a corresponding
   * `UnitType` tagged with `T0`.
   *
   * This is used to convert between `Integrated` <-> `Surface` units.
   */
  trait TagConverter[T, T0] {
    def convert(unit: UnitType @@ T): UnitType @@ T0
  }

  // `TagConverter` based on a `Map`.

  protected class MapConverter[T, T0](map: Map[UnitType @@ T, UnitType @@ T0])
      extends TagConverter[T, T0] {
    def convert(unit: UnitType @@ T): UnitType @@ T0 =
      map(unit)
  }

  // Identity type `TagConverter`.

  implicit def idConverter[T]: TagConverter[T, T] = new TagConverter[T, T] {
    override def convert(unit: UnitType @@ T): UnitType @@ T = unit
  }

  implicit object IntegratedToSurfaceBrightnessConverter
      extends MapConverter[Brightness[Integrated], Brightness[Surface]](
        Brightness.Integrated.all.zip(Brightness.Surface.all).toNem.toSortedMap
      )

  implicit object SurfaceToIntegratedBrightnessConverter
      extends MapConverter[Brightness[Surface], Brightness[Integrated]](
        Brightness.Surface.all.zip(Brightness.Integrated.all).toNem.toSortedMap
      )

  implicit object IntegratedToSurfaceLineFluxConverter
      extends MapConverter[LineFlux[Integrated], LineFlux[Surface]](
        LineFlux.Integrated.all.zip(LineFlux.Surface.all).toNem.toSortedMap
      )

  implicit object SurfaceToIntegratedLineFluxConverter
      extends MapConverter[LineFlux[Surface], LineFlux[Integrated]](
        LineFlux.Surface.all.zip(LineFlux.Integrated.all).toNem.toSortedMap
      )

  implicit object IntegratedToSurfaceFluxDensityContinuumConverter
      extends MapConverter[FluxDensityContinuum[Integrated], FluxDensityContinuum[Surface]](
        FluxDensityContinuum.Integrated.all.zip(FluxDensityContinuum.Surface.all).toNem.toSortedMap
      )

  implicit object SurfaceToIntegratedFluxDensityContinuumConverter
      extends MapConverter[FluxDensityContinuum[Surface], FluxDensityContinuum[Integrated]](
        FluxDensityContinuum.Surface.all.zip(FluxDensityContinuum.Integrated.all).toNem.toSortedMap
      )

  implicit class BrightnessUnitOps[T](val unit: UnitType @@ T) extends AnyVal {

    /** Convert `T`-tagged `UnitType` to a `T0`-tagged one. */
    def toTag[T0](implicit conv: TagConverter[T, T0]): UnitType @@ T0 =
      conv.convert(unit)
  }

  implicit class BrightnessQtyOps[N, T](val qty: Qty[N] @@ T) extends AnyVal {

    /** Convert `T`-tagged `Qty` to a `T0`-tagged one. */
    def toTag[T0](implicit conv: TagConverter[T, T0]): Qty[N] @@ T0 =
      conv.convert(Qty.unitTagged.get(qty)).withValueTagged(Qty.valueTagged.get(qty))
  }

}

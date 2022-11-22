// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package math

import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import lucuma.core.math.dimensional.*
import lucuma.core.math.units.*
import lucuma.core.util.Enumerated
import java.time.Instant

object BrightnessUnits {
  type Integrated
  type Surface

  trait Brightness[+T]
  trait LineFlux[+T]
  trait FluxDensityContinuum[+T]

  type BrightnessMeasure[T] = Measure[BigDecimal] Of Brightness[T]
  type BrightnessMeasureOverTime[T] = NonEmptyMap[Instant, Measure[BigDecimal] Of Brightness[T]]

  // Brightness Integrated
  implicit object VegaMagnitudeIsIntegratedBrightnessUnit
      extends TaggedUnit[VegaMagnitude, Brightness[Integrated]]
  implicit object ABMagnitudeIsIntegratedBrightnessUnit
      extends TaggedUnit[ABMagnitude, Brightness[Integrated]]
  implicit object JanskyIsIntegratedBrightnessUnit
      extends TaggedUnit[Jansky, Brightness[Integrated]]
  implicit object WattsPerMeter2MicrometerIsIntegratedBrightnessUnit
      extends TaggedUnit[WattsPerMeter2Micrometer, Brightness[Integrated]]
  implicit object ErgsPerSecondCentimeter2AngstromIsIntegratedBrightnessUnit
      extends TaggedUnit[ErgsPerSecondCentimeter2Angstrom, Brightness[Integrated]]
  implicit object ErgsPerSecondCentimeter2HertzIsIntegratedBrightnessUnit
      extends TaggedUnit[ErgsPerSecondCentimeter2Hertz, Brightness[Integrated]]

  // Brightness Surface
  implicit object VegaMagnitudePerArcsec2IsSurfaceBrightnessUnit
      extends TaggedUnit[VegaMagnitudePerArcsec2, Brightness[Surface]]
  implicit object ABMagnitudePerArcsec2IsSurfaceBrightnessUnit
      extends TaggedUnit[ABMagnitudePerArcsec2, Brightness[Surface]]
  implicit object JanskyPerArcsec2IsSurfaceBrightnessUnit
      extends TaggedUnit[JanskyPerArcsec2, Brightness[Surface]]
  implicit object WattsPerMeter2MicrometerArcsec2IsSurfaceBrightnessUnit
      extends TaggedUnit[WattsPerMeter2MicrometerArcsec2, Brightness[Surface]]
  implicit object ErgsPerSecondCentimeter2AngstromArcsec2IsSurfaceBrightnessUnit
      extends TaggedUnit[ErgsPerSecondCentimeter2AngstromArcsec2, Brightness[Surface]]
  implicit object ErgsPerSecondCentimeter2HertzArcsec2IsSurfaceBrightnessUnit
      extends TaggedUnit[ErgsPerSecondCentimeter2HertzArcsec2, Brightness[Surface]]

  // Line Flux Integrated
  implicit object WattsPerMeter2IsIntegratedLineFluxUnit
      extends TaggedUnit[WattsPerMeter2, LineFlux[Integrated]]
  implicit object ErgsPerSecondCentimeter2IsIntegratedLineFluxUnit
      extends TaggedUnit[ErgsPerSecondCentimeter2, LineFlux[Integrated]]

  // Line Flux Surface
  implicit object WattsPerMeter2Arcsec2IsSurfaceLineFluxUnit
      extends TaggedUnit[WattsPerMeter2Arcsec2, LineFlux[Surface]]
  implicit object ErgsPerSecondCentimeter2Arcsec2IsSurfaceLineFluxUnit
      extends TaggedUnit[ErgsPerSecondCentimeter2Arcsec2, LineFlux[Surface]]

  // Flux Density Continuum Integrated
  implicit object WattsPerMeter2MicrometerIsIntegratedFluxDensityContinuumUnit
      extends TaggedUnit[WattsPerMeter2Micrometer, FluxDensityContinuum[Integrated]]
  implicit object ErgsPerSecondCentimeter2AngstromIsIntegratedFluxDensityContinuumUnit
      extends TaggedUnit[ErgsPerSecondCentimeter2Angstrom, FluxDensityContinuum[Integrated]]

  // Flux Density Continuum Surface
  implicit object WattsPerMeter2MicrometerArcsec2IsSurfaceFluxDensityContinuumUnit
      extends TaggedUnit[WattsPerMeter2MicrometerArcsec2, FluxDensityContinuum[Surface]]
  implicit object ErgsPerSecondCentimeter2AngstromArcsec2IsSurfaceFluxDensityContinuumUnit
      extends TaggedUnit[ErgsPerSecondCentimeter2AngstromArcsec2, FluxDensityContinuum[Surface]]

  object Brightness {
    object Integrated {

      val all: NonEmptyList[Units Of Brightness[Integrated]] =
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

      val all: NonEmptyList[Units Of Brightness[Surface]] =
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
      val all: NonEmptyList[Units Of LineFlux[Integrated]] =
        NonEmptyList
          .of(
            WattsPerMeter2IsIntegratedLineFluxUnit,
            ErgsPerSecondCentimeter2IsIntegratedLineFluxUnit
          )
          .map(_.unit)
    }

    object Surface {
      val all: NonEmptyList[Units Of LineFlux[Surface]] =
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
      val all: NonEmptyList[Units Of FluxDensityContinuum[Integrated]] =
        NonEmptyList
          .of(
            WattsPerMeter2MicrometerIsIntegratedFluxDensityContinuumUnit,
            ErgsPerSecondCentimeter2AngstromIsIntegratedFluxDensityContinuumUnit
          )
          .map(_.unit)
    }

    object Surface {
      val all: NonEmptyList[Units Of FluxDensityContinuum[Surface]] =
        NonEmptyList
          .of(
            WattsPerMeter2MicrometerArcsec2IsSurfaceFluxDensityContinuumUnit,
            ErgsPerSecondCentimeter2AngstromArcsec2IsSurfaceFluxDensityContinuumUnit
          )
          .map(_.unit)
    }

  }

  // Enumerated instances

  private def enumTaggedUnit[T](
    allList: NonEmptyList[Units Of T]
  ): Enumerated[Units Of T] =
    Enumerated.fromNEL(allList).withTag(_.serialized)

  implicit val enumBrightnessIntegrated: Enumerated[Units Of Brightness[Integrated]] =
    enumTaggedUnit[Brightness[Integrated]](Brightness.Integrated.all)

  implicit val enumBrightnessSurface: Enumerated[Units Of Brightness[Surface]] =
    enumTaggedUnit[Brightness[Surface]](Brightness.Surface.all)

  implicit val enumLineFluxIntegrated: Enumerated[Units Of LineFlux[Integrated]] =
    enumTaggedUnit[LineFlux[Integrated]](LineFlux.Integrated.all)

  implicit val enumLineFluxSurface: Enumerated[Units Of LineFlux[Surface]] =
    enumTaggedUnit[LineFlux[Surface]](LineFlux.Surface.all)

  implicit val enumFluxDensityContinuumIntegrated
    : Enumerated[Units Of FluxDensityContinuum[Integrated]] =
    enumTaggedUnit[FluxDensityContinuum[Integrated]](FluxDensityContinuum.Integrated.all)

  implicit val enumFluxDensityContinuumSurface: Enumerated[Units Of FluxDensityContinuum[Surface]] =
    enumTaggedUnit[FluxDensityContinuum[Surface]](FluxDensityContinuum.Surface.all)

  /**
   * Typeclass providing a conversion between a `Units` tagged with `T` to a corresponding `Units`
   * tagged with `T0`.
   *
   * This is used to convert between `Integrated` <-> `Surface` units.
   */
  trait TagConverter[T, T0] {
    def convert(units: Units Of T): Units Of T0
  }

  // `TagConverter` based on a `Map`.

  protected class MapConverter[T, T0](map: Map[Units Of T, Units Of T0])
      extends TagConverter[T, T0] {
    def convert(units: Units Of T): Units Of T0 =
      map(units)
  }

  // Identity type `TagConverter`.

  implicit def idConverter[T]: TagConverter[T, T] = new TagConverter[T, T] {
    override def convert(units: Units Of T): Units Of T = units
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

  implicit class TaggedUnitOps[T](private val units: Units Of T) extends AnyVal {

    /** Convert `T`-tagged `Units` to a `T0`-tagged ones. */
    def toTag[T0](implicit conv: TagConverter[T, T0]): Units Of T0 =
      conv.convert(units)
  }

  implicit class TaggedMeasureOps[N, T](private val measure: Measure[N] Of T) extends AnyVal {

    /** Convert `T`-tagged `Measure` to a `T0`-tagged one. */
    def toTag[T0](implicit conv: TagConverter[T, T0]): Measure[N] Of T0 =
      conv
        .convert(Measure.unitsTagged.get(measure))
        .withValueTagged(Measure.valueTagged.get(measure), Measure.errorTagged.get(measure))
  }

}

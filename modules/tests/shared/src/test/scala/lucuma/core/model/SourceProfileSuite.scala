// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Order._
import cats.kernel.laws.discipline._
import eu.timepit.refined.cats._
import lucuma.core.enum.Band
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.dimensional.arb.ArbMeasure
import lucuma.core.model.arb._
import lucuma.core.util.arb.ArbEnumerated
import monocle.law.discipline._
import munit._
import coulomb._
import lucuma.core.math.units._
import scala.collection.immutable.SortedMap
import lucuma.core.enum.StellarLibrarySpectrum
import lucuma.core.math.BrightnessValue
import lucuma.core.math.BrightnessUnits._

final class SourceProfileSuite extends DisciplineSuite {
  import ArbSourceProfile._
  import ArbSpectralDefinition._
  import ArbEnumerated._
  import ArbGaussianSource._
  import ArbUnnormalizedSED._
  import ArbBandBrightness._
  import ArbEmissionLine._
  import ArbRefined._
  import ArbMeasure._

  // Conversions
  val sd1Integrated: SpectralDefinition[Integrated] = {
    val b: BandBrightness[Integrated] =
      BandBrightness[Integrated](BrightnessValue.fromDouble(10.0), Band.R)

    SpectralDefinition.BandNormalized(
      UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0I),
      SortedMap(b.band -> b)
    )
  }

  val sd1Surface: SpectralDefinition[Surface] = {
    val b: BandBrightness[Surface] =
      BandBrightness[Surface](BrightnessValue.fromDouble(10.0), Band.R)

    SpectralDefinition.BandNormalized(
      UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0I),
      SortedMap(b.band -> b)
    )
  }

  val sd2Integrated: SpectralDefinition[Integrated] = {
    val line: EmissionLine[Integrated] =
      EmissionLine(
        Wavelength.Min,
        PosBigDecimalOne.withUnit[KilometersPerSecond],
        WattsPerMeter2IsIntegratedLineFluxUnit.unit.withValueTagged(PosBigDecimalOne)
      )

    SpectralDefinition.EmissionLines(
      SortedMap(line.wavelength -> line),
      WattsPerMeter2MicrometerIsIntegratedFluxDensityContinuumUnit.unit
        .withValueTagged(PosBigDecimalOne)
    )
  }

  val sd2Surface: SpectralDefinition[Surface] = {
    val line: EmissionLine[Surface] =
      EmissionLine(
        Wavelength.Min,
        PosBigDecimalOne.withUnit[KilometersPerSecond],
        WattsPerMeter2Arcsec2IsSurfaceLineFluxUnit.unit.withValueTagged(PosBigDecimalOne)
      )

    SpectralDefinition.EmissionLines(
      SortedMap(line.wavelength -> line),
      WattsPerMeter2MicrometerArcsec2IsSurfaceFluxDensityContinuumUnit.unit
        .withValueTagged(PosBigDecimalOne)
    )
  }

  val point1    = SourceProfile.Point(sd1Integrated)
  val point2    = SourceProfile.Point(sd2Integrated)
  val uniform1  = SourceProfile.Uniform(sd1Surface)
  val uniform2  = SourceProfile.Uniform(sd2Surface)
  val gaussian1 = SourceProfile.Gaussian(GaussianSource.Zero, sd1Integrated)
  val gaussian2 = SourceProfile.Gaussian(GaussianSource.Zero, sd2Integrated)

  test("toPoint") {
    assertEquals(point1.toPoint, point1)
    assertEquals(point2.toPoint, point2)
    assertEquals(uniform1.toPoint, point1)
    assertEquals(uniform2.toPoint, point2)
    assertEquals(gaussian1.toPoint, point1)
    assertEquals(gaussian2.toPoint, point2)
  }

  test("toUniform") {
    assertEquals(point1.toUniform, uniform1)
    assertEquals(point2.toUniform, uniform2)
    assertEquals(uniform1.toUniform, uniform1)
    assertEquals(uniform2.toUniform, uniform2)
    assertEquals(gaussian1.toUniform, uniform1)
    assertEquals(gaussian2.toUniform, uniform2)
  }

  test("toGaussian") {
    assertEquals(point1.toGaussian, gaussian1)
    assertEquals(point2.toGaussian, gaussian2)
    assertEquals(uniform1.toGaussian, gaussian1)
    assertEquals(uniform2.toGaussian, gaussian2)
    assertEquals(gaussian1.toGaussian, gaussian1)
    assertEquals(gaussian2.toGaussian, gaussian2)
  }

  // Laws for SourceProfile.Point
  checkAll("Eq[SourceProfile.Point]", EqTests[SourceProfile.Point].eqv)

  // Laws for SourceProfile.Uniform
  checkAll("Eq[SourceProfile.Uniform]", EqTests[SourceProfile.Uniform].eqv)
  checkAll(
    "SourceProfile.Uniform.spectralDefinition",
    LensTests(SourceProfile.Uniform.spectralDefinition)
  )

  // Laws for SourceProfile.Gaussian
  checkAll("Eq[SourceProfile.Gaussian]", EqTests[SourceProfile.Gaussian].eqv)
  checkAll("SourceProfile.Gaussian.source", LensTests(SourceProfile.Gaussian.source))
  checkAll(
    "SourceProfile.Gaussian.spectralDefinition",
    LensTests(SourceProfile.Gaussian.spectralDefinition)
  )

  // Laws for SourceProfile
  checkAll("Eq[SourceProfile]", EqTests[SourceProfile].eqv)
  checkAll("SourceProfile.point", PrismTests(SourceProfile.point))
  checkAll("SourceProfile.uniform", PrismTests(SourceProfile.uniform))
  checkAll("SourceProfile.gaussian", PrismTests(SourceProfile.gaussian))
  checkAll(
    "SourceProfile.integratedSpectralDefinition",
    OptionalTests(SourceProfile.integratedSpectralDefinition)
  )
  checkAll(
    "SourceProfile.surfaceSpectralDefinition",
    OptionalTests(SourceProfile.surfaceSpectralDefinition)
  )
  checkAll(
    "SourceProfile.gaussianSource",
    OptionalTests(SourceProfile.gaussianSource)
  )
  checkAll(
    "SourceProfile.integratedBandNormalizedSpectralDefinition",
    OptionalTests(SourceProfile.integratedBandNormalizedSpectralDefinition)
  )
  checkAll(
    "SourceProfile.surfaceBandNormalizedSpectralDefinition",
    OptionalTests(SourceProfile.surfaceBandNormalizedSpectralDefinition)
  )
  checkAll(
    "SourceProfile.integratedEmissionLinesSpectralDefinition",
    OptionalTests(SourceProfile.integratedEmissionLinesSpectralDefinition)
  )
  checkAll(
    "SourceProfile.surfaceEmissionLinesSpectralDefinition",
    OptionalTests(SourceProfile.surfaceEmissionLinesSpectralDefinition)
  )
  checkAll(
    "SourceProfile.unnormalizedSED",
    OptionalTests(SourceProfile.unnormalizedSED)
  )
  checkAll(
    "SourceProfile.integratedBandBrightnesses",
    OptionalTests(SourceProfile.integratedBandBrightnesses)
  )
  checkAll(
    "SourceProfile.surfaceBandBrightnesses",
    OptionalTests(SourceProfile.surfaceBandBrightnesses)
  )
  checkAll(
    "SourceProfile.integratedBandBrightnessesT",
    TraversalTests(SourceProfile.integratedBandBrightnessesT)
  )
  checkAll(
    "SourceProfile.surfaceBandBrightnessesT",
    TraversalTests(SourceProfile.surfaceBandBrightnessesT)
  )
  checkAll(
    "SourceProfile.integratedBandBrightnessIn",
    TraversalTests(SourceProfile.integratedBandBrightnessIn(Band.B))
  )
  checkAll(
    "SourceProfile.surfaceBandBrightnessIn",
    TraversalTests(SourceProfile.surfaceBandBrightnessIn(Band.B))
  )
  checkAll(
    "SourceProfile.integratedWavelengthLines",
    OptionalTests(SourceProfile.integratedWavelengthLines)
  )
  checkAll(
    "SourceProfile.surfaceWavelengthLines",
    OptionalTests(SourceProfile.surfaceWavelengthLines)
  )
  checkAll(
    "SourceProfile.integratedWavelengthLinesT",
    TraversalTests(SourceProfile.integratedWavelengthLinesT)
  )
  checkAll(
    "SourceProfile.surfaceWavelengthLinesT",
    TraversalTests(SourceProfile.surfaceWavelengthLinesT)
  )
  checkAll(
    "SourceProfile.integratedWavelengthLineIn",
    TraversalTests(
      SourceProfile.integratedWavelengthLineIn(Wavelength.fromPicometers.getOption(620000).get)
    )
  )
  checkAll(
    "SourceProfile.surfaceWavelengthLineIn",
    TraversalTests(
      SourceProfile.surfaceWavelengthLineIn(Wavelength.fromPicometers.getOption(620000).get)
    )
  )
  checkAll(
    "SourceProfile.integratedFluxDensityContinuum",
    OptionalTests(SourceProfile.integratedFluxDensityContinuum)
  )
  checkAll(
    "SourceProfile.surfaceFluxDensityContinuum",
    OptionalTests(SourceProfile.surfaceFluxDensityContinuum)
  )
}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Order._
import cats.kernel.laws.discipline._
import coulomb._
import eu.timepit.refined.cats._
import lucuma.core.enums.Band
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessUnits._
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbAngle
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.math.dimensional.arb.ArbMeasure
import lucuma.core.math.units._
import lucuma.core.model.arb._
import lucuma.core.util.arb.ArbCollection
import lucuma.core.util.arb.ArbEnumerated
import monocle.law.discipline._
import munit._

import scala.collection.immutable.SortedMap

final class SourceProfileSuite extends DisciplineSuite {
  import ArbAngle._
  import ArbEmissionLine._
  import ArbEnumerated._
  import ArbMeasure._
  import ArbRefined._
  import ArbSourceProfile._
  import ArbSpectralDefinition._
  import ArbUnnormalizedSED._
  import ArbWavelength._
  import ArbCollection._

  // Conversions
  val sd1Integrated: SpectralDefinition[Integrated] =
    SpectralDefinition.BandNormalized(
      UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0I),
      SortedMap(
        Band.R -> Band.R.defaultUnits[Integrated].withValueTagged(BigDecimal(10.0))
      )
    )

  val sd1Surface: SpectralDefinition[Surface] =
    SpectralDefinition.BandNormalized(
      UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0I),
      SortedMap(
        Band.R -> Band.R.defaultUnits[Surface].withValueTagged(BigDecimal(10.0))
      )
    )

  val sd2Integrated: SpectralDefinition[Integrated] =
    SpectralDefinition.EmissionLines(
      SortedMap(
        Wavelength.Min -> EmissionLine(
          PosBigDecimalOne.withUnit[KilometersPerSecond],
          WattsPerMeter2IsIntegratedLineFluxUnit.unit.withValueTagged(PosBigDecimalOne)
        )
      ),
      WattsPerMeter2MicrometerIsIntegratedFluxDensityContinuumUnit.unit
        .withValueTagged(PosBigDecimalOne)
    )

  val sd2Surface: SpectralDefinition[Surface] =
    SpectralDefinition.EmissionLines(
      SortedMap(
        Wavelength.Min -> EmissionLine(
          PosBigDecimalOne.withUnit[KilometersPerSecond],
          WattsPerMeter2Arcsec2IsSurfaceLineFluxUnit.unit.withValueTagged(PosBigDecimalOne)
        )
      ),
      WattsPerMeter2MicrometerArcsec2IsSurfaceFluxDensityContinuumUnit.unit
        .withValueTagged(PosBigDecimalOne)
    )

  val point1    = SourceProfile.Point(sd1Integrated)
  val point2    = SourceProfile.Point(sd2Integrated)
  val uniform1  = SourceProfile.Uniform(sd1Surface)
  val uniform2  = SourceProfile.Uniform(sd2Surface)
  val gaussian1 = SourceProfile.Gaussian(Angle.Angle0, sd1Integrated)
  val gaussian2 = SourceProfile.Gaussian(Angle.Angle0, sd2Integrated)

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
  checkAll("SourceProfile.Gaussian.fwhm", LensTests(SourceProfile.Gaussian.fwhm))
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
  checkAll("SourceProfile.fwhm", OptionalTests(SourceProfile.fwhm))
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
    "SourceProfile.integratedBrightnesses",
    OptionalTests(SourceProfile.integratedBrightnesses)
  )
  checkAll(
    "SourceProfile.surfaceBrightnesses",
    OptionalTests(SourceProfile.surfaceBrightnesses)
  )
  checkAll(
    "SourceProfile.integratedBrightnessesT",
    TraversalTests(SourceProfile.integratedBrightnessesT)
  )
  checkAll(
    "SourceProfile.surfaceBrightnessesT",
    TraversalTests(SourceProfile.surfaceBrightnessesT)
  )
  checkAll(
    "SourceProfile.integratedBrightnessIn",
    TraversalTests(SourceProfile.integratedBrightnessIn(Band.B))
  )
  checkAll(
    "SourceProfile.surfaceBrightnessIn",
    TraversalTests(SourceProfile.surfaceBrightnessIn(Band.B))
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

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Order.*
import cats.kernel.laws.discipline.*
import cats.syntax.all.*
import coulomb.*
import coulomb.syntax.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.numeric.Positive
import lucuma.core.enums.Band
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.math.Angle
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.FluxDensityContinuumValue
import lucuma.core.math.LineFluxValue
import lucuma.core.math.LineWidthValue
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbAngle
import lucuma.core.math.arb.ArbFluxDensityContinuumValue
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.math.dimensional.arb.ArbMeasure
import lucuma.core.math.units.*
import lucuma.core.model.arb.*
import lucuma.core.util.arb.ArbCollection
import lucuma.core.util.arb.ArbEnumerated
import lucuma.refined.*
import monocle.law.discipline.*
import munit.*
import lucuma.core.util.arb.ArbNewType
import lucuma.core.math.arb.ArbRefined
import eu.timepit.refined.scalacheck.numeric.*


import scala.collection.immutable.SortedMap

final class SourceProfileSuite extends DisciplineSuite {
  import ArbAngle.given
  import ArbEmissionLine.given
  import ArbEnumerated.given
  import ArbFluxDensityContinuumValue.given
  import ArbMeasure.given
  import ArbNewType.given
  import ArbRefined.given
  import ArbSourceProfile.given
  import ArbSpectralDefinition.given
  import ArbUnnormalizedSED.given
  import ArbWavelength.given
  import ArbCollection.given

  val sd1Brightnesses = SortedMap[Band, BrightnessMeasure[Integrated]](
      Band.R -> Band.R.defaultUnits[Integrated].withValueTagged(BrightnessValue.unsafeFrom(10.0))
    )

  val sd1BrightnessesB = sd1Brightnesses ++ SortedMap[Band, BrightnessMeasure[Integrated]](
      Band.SloanR -> Band.U.defaultUnits[Integrated].withValueTagged(BrightnessValue.unsafeFrom(12.0))
    )

  // Conversions
  val sd1Integrated: SpectralDefinition[Integrated] =
    SpectralDefinition.BandNormalized(
      UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0I).some,
      sd1Brightnesses
    )

  val sd1Surface: SpectralDefinition[Surface] =
    SpectralDefinition.BandNormalized(
      UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0I).some,
      SortedMap(
        Band.R -> Band.R.defaultUnits[Surface].withValueTagged(BrightnessValue.unsafeFrom(10.0))
      )
    )

  val sd2Integrated: SpectralDefinition[Integrated] =
    SpectralDefinition.EmissionLines(
      SortedMap(
        Wavelength.Min -> EmissionLine(
          LineWidthValue.unsafeFrom(1).withUnit[KilometersPerSecond],
          WattsPerMeter2IsIntegratedLineFluxUnit.unit.withValueTagged(LineFluxValue.unsafeFrom(1))
        )
      ),
      WattsPerMeter2MicrometerIsIntegratedFluxDensityContinuumUnit.unit
        .withValueTagged(FluxDensityContinuumValue.unsafeFrom(1))
    )

  val sd2Surface: SpectralDefinition[Surface] =
    SpectralDefinition.EmissionLines(
      SortedMap(
        Wavelength.Min -> EmissionLine(
          LineWidthValue.unsafeFrom(1).withUnit[KilometersPerSecond],
          WattsPerMeter2Arcsec2IsSurfaceLineFluxUnit.unit.withValueTagged(LineFluxValue.unsafeFrom(1))
        )
      ),
      WattsPerMeter2MicrometerArcsec2IsSurfaceFluxDensityContinuumUnit.unit
        .withValueTagged(FluxDensityContinuumValue.unsafeFrom(1))
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

  test("nearestBand") {
    val wv = Wavelength(578000.refined[Positive])
    assert(point1.nearestBand(wv).contains_(Band.R))
    assert(point2.nearestBand(wv).isEmpty)
    assert(uniform1.nearestBand(wv).contains_(Band.R))
    assert(uniform2.nearestBand(wv).isEmpty)
    assert(gaussian1.nearestBand(wv).contains_(Band.R))
    assert(gaussian2.nearestBand(wv).isEmpty)
  }

  test("nearestLine") {
    val wv = Wavelength(578000.refined[Positive])
    assert(point1.nearestLine(wv).isEmpty)
    assert(point2.nearestLine(wv).contains_(Wavelength.Min))
    assert(uniform1.nearestLine(wv).isEmpty)
    assert(uniform2.nearestLine(wv).contains_(Wavelength.Min))
    assert(gaussian1.nearestLine(wv).isEmpty)
    assert(gaussian2.nearestLine(wv).contains_(Wavelength.Min))
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
      SourceProfile.integratedWavelengthLineIn(Wavelength.intPicometers.getOption(620000).get)
    )
  )
  checkAll(
    "SourceProfile.surfaceWavelengthLineIn",
    TraversalTests(
      SourceProfile.surfaceWavelengthLineIn(Wavelength.intPicometers.getOption(620000).get)
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

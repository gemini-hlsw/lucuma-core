// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import lucuma.core.model.arb._
import monocle.law.discipline._
import munit._
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.enum.Band
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.dimensional.arb.ArbQty
import eu.timepit.refined.cats._

final class SourceProfileSuite extends DisciplineSuite {
  import ArbSourceProfile._
  import ArbSpectralDefinition._
  import ArbEnumerated._
  import ArbGaussianSource._
  import ArbUSED._
  import ArbBandBrightness._
  import ArbEmissionLine._
  import ArbRefined._
  import ArbQty._

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
    "SourceProfile.integratedUSED",
    OptionalTests(SourceProfile.integratedUSED)
  )
  checkAll(
    "SourceProfile.surfaceUSED",
    OptionalTests(SourceProfile.surfaceUSED)
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

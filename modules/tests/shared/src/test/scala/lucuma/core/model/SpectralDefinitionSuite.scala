// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import lucuma.core.model.arb._
import monocle.law.discipline._
import munit._
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.enum.Band
import lucuma.core.math.BrightnessUnits
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.dimensional.arb.ArbQty
import eu.timepit.refined.cats._

final class SpectralDefinitionSuite extends DisciplineSuite {
  import ArbUnnormalizedSpectralEnergyDistribution._
  import ArbEnumerated._
  import ArbBandBrightness._
  import BrightnessUnits._
  import ArbSpectralDefinition._
  import ArbRefined._
  import ArbQty._

  // Laws for SpectralDefinition.BandNormalized
  checkAll(
    "Eq[SpectralDefinition.BandNormalized[Integrated]]",
    EqTests[SpectralDefinition.BandNormalized[Integrated]].eqv
  )
  checkAll(
    "Eq[SpectralDefinition.BandNormalized[Surface]]",
    EqTests[SpectralDefinition.BandNormalized[Surface]].eqv
  )
  checkAll(
    "SpectralDefinition.BandNormalized.sed[Integrated]",
    LensTests(SpectralDefinition.BandNormalized.sed[Integrated])
  )
  checkAll(
    "SpectralDefinition.BandNormalized.sed[Surface]",
    LensTests(SpectralDefinition.BandNormalized.sed[Surface])
  )
  checkAll(
    "SpectralDefinition.BandNormalized.brightnesses[Integrated]",
    LensTests(SpectralDefinition.BandNormalized.brightnesses[Integrated])
  )
  checkAll(
    "SpectralDefinition.BandNormalized.brightnesses[Surface]",
    LensTests(SpectralDefinition.BandNormalized.brightnesses[Surface])
  )
  checkAll(
    "SpectralDefinition.BandNormalized.brightnessesT[Integrated]",
    TraversalTests(SpectralDefinition.BandNormalized.brightnessesT[Integrated])
  )
  checkAll(
    "SpectralDefinition.BandNormalized.brightnessesT[Surface]",
    TraversalTests(SpectralDefinition.BandNormalized.brightnessesT[Surface])
  )
  checkAll(
    "SpectralDefinition.BandNormalized.brightnessIn[Integrated]",
    TraversalTests(SpectralDefinition.BandNormalized.brightnessIn[Integrated](Band.B))
  )
  checkAll(
    "SpectralDefinition.BandNormalized.brightnessIn[Surface]",
    TraversalTests(SpectralDefinition.BandNormalized.brightnessIn[Surface](Band.B))
  )

  // Laws for SpectralDefinition.EmissionLines
  checkAll(
    "Eq[SpectralDefinition.EmissionLines[Integrated]]",
    EqTests[SpectralDefinition.EmissionLines[Integrated]].eqv
  )
  checkAll(
    "Eq[SpectralDefinition.EmissionLines[Surface]]",
    EqTests[SpectralDefinition.EmissionLines[Surface]].eqv
  )
  checkAll(
    "SpectralDefinition.EmissionLines.lines[Integrated]",
    LensTests(SpectralDefinition.EmissionLines.lines[Integrated])
  )
  checkAll(
    "SpectralDefinition.EmissionLines.lines[Surface]",
    LensTests(SpectralDefinition.EmissionLines.lines[Surface])
  )
  checkAll(
    "SpectralDefinition.EmissionLines.fluxDensityContinuum[Integrated]",
    LensTests(SpectralDefinition.EmissionLines.fluxDensityContinuum[Integrated])
  )
  checkAll(
    "SpectralDefinition.EmissionLines.fluxDensityContinuum[Surface]",
    LensTests(SpectralDefinition.EmissionLines.fluxDensityContinuum[Surface])
  )

  // Laws for SpectralDefinition
  checkAll("Eq[SpectralDefinition[Integrated]]", EqTests[SpectralDefinition[Integrated]].eqv)
  checkAll("Eq[SpectralDefinition[Surface]]", EqTests[SpectralDefinition[Surface]].eqv)
  checkAll(
    "SpectralDefinition.bandNormalized[Integrated]",
    PrismTests(SpectralDefinition.bandNormalized[Integrated])
  )
  checkAll(
    "SpectralDefinition.bandNormalized[Surface]",
    PrismTests(SpectralDefinition.bandNormalized[Surface])
  )
  checkAll(
    "SpectralDefinition.emissionLines[Integrated]",
    PrismTests(SpectralDefinition.emissionLines[Integrated])
  )
  checkAll(
    "SpectralDefinition.emissionLines[Surface]",
    PrismTests(SpectralDefinition.emissionLines[Surface])
  )

}

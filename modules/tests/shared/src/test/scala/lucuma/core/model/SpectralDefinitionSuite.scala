// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Order.*
import cats.data.NonEmptyMap
import cats.kernel.laws.discipline.*
import cats.laws.discipline.arbitrary.*
import org.typelevel.cats.time.instantInstances
import coulomb.*
import coulomb.syntax.*
import eu.timepit.refined.cats.*
import lucuma.core.enums.Band
import lucuma.core.enums.StellarLibrarySpectrum
import lucuma.core.math.BrightnessUnits
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.math.dimensional.arb.ArbMeasure
import lucuma.core.math.units.*
import lucuma.core.model.arb.*
import lucuma.core.util.Timestamp
import lucuma.core.util.arb.ArbCollection
import lucuma.core.util.arb.ArbEnumerated
import monocle.law.discipline.*
import munit.*

import scala.collection.immutable.SortedMap
import java.time.Instant

final class SpectralDefinitionSuite extends DisciplineSuite {
  import ArbUnnormalizedSED.*
  import ArbEnumerated.*
  import BrightnessUnits.*
  import ArbSpectralDefinition.*
  import ArbRefined.*
  import ArbMeasure.*
  import ArbEmissionLine.*
  import ArbCollection.*
  import ArbWavelength.*

  // Brightness type conversions
  val sd1Integrated: SpectralDefinition[Integrated] =
    SpectralDefinition.BandNormalized(
      UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0I),
      SortedMap(
        Band.R -> NonEmptyMap.of(Instant.MIN -> Band.R.defaultUnits[Integrated].withValueTagged(BigDecimal(10.0)))
      )
    )

  val sd1Surface: SpectralDefinition[Surface] =
    SpectralDefinition.BandNormalized(
      UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0I),
      SortedMap(
        Band.R -> NonEmptyMap.of(Instant.MIN -> Band.R.defaultUnits[Surface].withValueTagged(BigDecimal(10.0)))
      )
    )

  val sd2Integrated: SpectralDefinition[Integrated] =
    SpectralDefinition.EmissionLines(
      SortedMap(
        Wavelength.Min -> EmissionLine(
          PosBigDecimalOne.withUnit[KilometersPerSecond],
          NonEmptyMap.of(Instant.MIN -> ErgsPerSecondCentimeter2IsIntegratedLineFluxUnit.unit.withValueTagged(PosBigDecimalOne))
        )
      ),
      WattsPerMeter2MicrometerIsIntegratedFluxDensityContinuumUnit.unit.withValueTagged(
        PosBigDecimalOne
      )
    )

  val sd2Surface: SpectralDefinition[Surface] =
    SpectralDefinition.EmissionLines(
      SortedMap(
        Wavelength.Min -> EmissionLine(
          PosBigDecimalOne.withUnit[KilometersPerSecond],
          NonEmptyMap.of(Instant.MIN ->
            ErgsPerSecondCentimeter2Arcsec2IsSurfaceLineFluxUnit.unit.withValueTagged(PosBigDecimalOne))
        )
      ),
      WattsPerMeter2MicrometerArcsec2IsSurfaceFluxDensityContinuumUnit.unit.withValueTagged(
        PosBigDecimalOne
      )
    )

  test("Brightness type conversion Integrated -> Surface") {
    assertEquals(sd1Integrated.to[Surface], sd1Surface)
    assertEquals(sd2Integrated.to[Surface], sd2Surface)
  }

  test("Brightness identity type conversion") {
    assertEquals(sd1Surface.to[Integrated], sd1Integrated)
    assertEquals(sd2Surface.to[Integrated], sd2Integrated)
  }

  test("Brightness type conversion roundtrip") {
    assertEquals(sd1Surface.to[Integrated].to[Surface], sd1Surface)
    assertEquals(sd2Surface.to[Integrated].to[Surface], sd2Surface)
  }

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
    "SpectralDefinition.EmissionLines.linesT[Integrated]",
    TraversalTests(SpectralDefinition.EmissionLines.linesT[Integrated])
  )
  checkAll(
    "SpectralDefinition.EmissionLines.linesT[Surface]",
    TraversalTests(SpectralDefinition.EmissionLines.linesT[Surface])
  )
  checkAll(
    "SpectralDefinition.EmissionLines.lines[Integrated]",
    TraversalTests(SpectralDefinition.EmissionLines.lineIn[Integrated](RedWavelength))
  )
  checkAll(
    "SpectralDefinition.EmissionLines.lines[Surface]",
    TraversalTests(SpectralDefinition.EmissionLines.lineIn[Surface](RedWavelength))
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

  checkAll(
    "SpectralDefinition.unnormalizedSED[Integrated]",
    OptionalTests(SpectralDefinition.unnormalizedSED[Integrated])
  )
  checkAll(
    "SpectralDefinition.unnormalizedSED[Surface]",
    OptionalTests(SpectralDefinition.unnormalizedSED[Surface])
  )
  checkAll(
    "SpectralDefinition.brightnesses[Integrated]",
    OptionalTests(SpectralDefinition.brightnesses[Integrated])
  )
  checkAll(
    "SpectralDefinition.brightnesses[Surface]",
    OptionalTests(SpectralDefinition.brightnesses[Surface])
  )
  checkAll(
    "SpectralDefinition.brightnessesT[Integrated]",
    TraversalTests(SpectralDefinition.brightnessesT[Integrated])
  )
  checkAll(
    "SpectralDefinition.brightnessesT[Surface]",
    TraversalTests(SpectralDefinition.brightnessesT[Surface])
  )
  checkAll(
    "SpectralDefinition.brightnessIn[Integrated]",
    TraversalTests(SpectralDefinition.brightnessIn[Integrated](Band.B))
  )
  checkAll(
    "SpectralDefinition.brightnessIn[Surface]",
    TraversalTests(SpectralDefinition.brightnessIn[Surface](Band.B))
  )
  checkAll(
    "SpectralDefinition.wavelengthLines[Integrated]",
    OptionalTests(SpectralDefinition.wavelengthLines[Integrated])
  )
  checkAll(
    "SpectralDefinition.wavelengthLines[Surface]",
    OptionalTests(SpectralDefinition.wavelengthLines[Surface])
  )
  checkAll(
    "SpectralDefinition.wavelengthLinesT[Integrated]",
    TraversalTests(SpectralDefinition.wavelengthLinesT[Integrated])
  )
  checkAll(
    "SpectralDefinition.wavelengthLinesT[Surface]",
    TraversalTests(SpectralDefinition.wavelengthLinesT[Surface])
  )
  checkAll(
    "SpectralDefinition.wavelengthLineIn[Integrated]",
    TraversalTests(SpectralDefinition.wavelengthLineIn[Integrated](RedWavelength))
  )
  checkAll(
    "SpectralDefinition.wavelengthLineIn[Surface]",
    TraversalTests(SpectralDefinition.wavelengthLineIn[Surface](RedWavelength))
  )
  checkAll(
    "SpectralDefinition.fluxDensityContinuum[Integrated]",
    OptionalTests(SpectralDefinition.fluxDensityContinuum[Integrated])
  )
  checkAll(
    "SpectralDefinition.fluxDensityContinuum[Surface]",
    OptionalTests(SpectralDefinition.fluxDensityContinuum[Surface])
  )
}

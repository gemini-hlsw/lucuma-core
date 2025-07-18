// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Order.*
import cats.kernel.laws.discipline.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.scalacheck.numeric.*
import eu.timepit.refined.scalacheck.string.*
import lucuma.core.enums.Band
import lucuma.core.math.arb.*
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.math.dimensional.arb.ArbMeasure
import lucuma.core.model.arb.*
import lucuma.core.util.arb.*
import lucuma.core.util.arb.ArbCollection
import lucuma.core.util.arb.ArbNewType
import lucuma.core.util.laws.GidTests
import monocle.law.discipline.*
import munit.*

class TargetSuite extends DisciplineSuite {
  import ArbAngle.given
  import ArbCatalogInfo.given
  import ArbCollection.given
  import ArbCoordinates.given
  import ArbDeclination.given
  import ArbEnumerated.given
  import ArbEpoch.given
  import ArbEphemerisKey.given
  import ArbEmissionLine.given
  import ArbGid.given
  import ArbMeasure.given
  import ArbNewType.given
  import ArbParallax.given
  import ArbProperMotion.given
  import ArbRadialVelocity.given
  import ArbRefined.given
  import ArbRegion.given
  import ArbRightAscension.given
  import ArbSiderealTracking.given
  import ArbSourceProfile.given
  import ArbSpectralDefinition.given
  import ArbTarget.given
  import ArbUnnormalizedSED.given
  import ArbWavelength.given
  import ArbWavelength.*
  import Target.*

  // Laws for Target.Sidereal
  checkAll("Eq[Target.Sidereal]", EqTests[Target.Sidereal].eqv)
  checkAll(
    "Target.Sidereal.TrackOrder",
    OrderTests[Target.Sidereal](using Target.Sidereal.TrackOrder).order
  )
  checkAll(
    "Target.Sidereal.NameOrder",
    OrderTests[Target.Sidereal](using Target.Sidereal.NameOrder).order
  )
  checkAll("Target.Sidereal.name", LensTests(Target.Sidereal.name))
  checkAll("Target.Sidereal.tracking", LensTests(Target.Sidereal.tracking))
  checkAll("Target.Sidereal.parallax", LensTests(Target.Sidereal.parallax))
  checkAll("Target.Sidereal.radialVelocity", LensTests(Target.Sidereal.radialVelocity))
  checkAll("Target.Sidereal.baseCoordinates", LensTests(Target.Sidereal.baseCoordinates))
  checkAll("Target.Sidereal.baseRA", LensTests(Target.Sidereal.baseRA))
  checkAll("Target.Sidereal.baseDec", LensTests(Target.Sidereal.baseDec))
  checkAll("Target.Sidereal.catalogInfo", LensTests(Target.Sidereal.catalogInfo))
  checkAll("Target.Sidereal.epoch", LensTests(Target.Sidereal.epoch))
  checkAll("Target.Sidereal.properMotion", LensTests(Target.Sidereal.properMotion))
  checkAll("Target.Sidereal.properMotionRA", OptionalTests(Target.Sidereal.properMotionRA))
  checkAll("Target.Sidereal.properMotionDec", OptionalTests(Target.Sidereal.properMotionDec))
  checkAll("Target.Sidereal.sourceProfile", LensTests(Target.Sidereal.sourceProfile))
  checkAll(
    "Target.Sidereal.integratedSpectralDefinition",
    OptionalTests(Target.Sidereal.integratedSpectralDefinition)
  )
  checkAll(
    "Target.Sidereal.surfaceSpectralDefinition",
    OptionalTests(Target.Sidereal.surfaceSpectralDefinition)
  )
  checkAll("Target.Sidereal.fwhm", OptionalTests(Target.Sidereal.fwhm))
  checkAll(
    "Target.Sidereal.integratedBandNormalizedSpectralDefinition",
    OptionalTests(Target.Sidereal.integratedBandNormalizedSpectralDefinition)
  )
  checkAll(
    "Target.Sidereal.surfaceBandNormalizedSpectralDefinition",
    OptionalTests(Target.Sidereal.surfaceBandNormalizedSpectralDefinition)
  )
  checkAll(
    "Target.Sidereal.integratedEmissionLinesSpectralDefinition",
    OptionalTests(Target.Sidereal.integratedEmissionLinesSpectralDefinition)
  )
  checkAll(
    "Target.Sidereal.surfaceEmissionLinesSpectralDefinition",
    OptionalTests(Target.Sidereal.surfaceEmissionLinesSpectralDefinition)
  )
  checkAll(
    "Target.Sidereal.unnormalizedSED",
    OptionalTests(Target.Sidereal.unnormalizedSED)
  )
  checkAll(
    "Target.Sidereal.integratedBrightnesses",
    OptionalTests(Target.Sidereal.integratedBrightnesses)
  )
  checkAll(
    "Target.Sidereal.surfaceBrightnesses",
    OptionalTests(Target.Sidereal.surfaceBrightnesses)
  )
  checkAll(
    "Target.Sidereal.integratedBrightnessesT",
    TraversalTests(Target.Sidereal.integratedBrightnessesT)
  )
  checkAll(
    "Target.Sidereal.surfaceBrightnessesT",
    TraversalTests(Target.Sidereal.surfaceBrightnessesT)
  )
  checkAll(
    "Target.Sidereal.integratedBrightnessIn",
    TraversalTests(Target.Sidereal.integratedBrightnessIn(Band.B))
  )
  checkAll(
    "Target.Sidereal.surfaceBrightnessIn",
    TraversalTests(Target.Sidereal.surfaceBrightnessIn(Band.B))
  )
  checkAll(
    "Target.Sidereal.integratedWavelengthLines",
    OptionalTests(Target.Sidereal.integratedWavelengthLines)
  )
  checkAll(
    "Target.Sidereal.surfaceWavelengthLines",
    OptionalTests(Target.Sidereal.surfaceWavelengthLines)
  )
  checkAll(
    "Target.Sidereal.integratedWavelengthLinesT",
    TraversalTests(Target.Sidereal.integratedWavelengthLinesT)
  )
  checkAll(
    "Target.Sidereal.surfaceWavelengthLinesT",
    TraversalTests(Target.Sidereal.surfaceWavelengthLinesT)
  )
  checkAll(
    "Target.Sidereal.integratedWavelengthLineIn",
    TraversalTests(
      Target.Sidereal.integratedWavelengthLineIn(RedWavelength)
    )
  )
  checkAll(
    "Target.Sidereal.surfaceWavelengthLineIn",
    TraversalTests(
      Target.Sidereal.surfaceWavelengthLineIn(RedWavelength)
    )
  )
  checkAll(
    "Target.Sidereal.integratedFluxDensityContinuum",
    OptionalTests(Target.Sidereal.integratedFluxDensityContinuum)
  )
  checkAll(
    "Target.Sidereal.surfaceFluxDensityContinuum",
    OptionalTests(Target.Sidereal.surfaceFluxDensityContinuum)
  )
  checkAll("Target.Sidereal.catalogInfo", LensTests(Target.Sidereal.catalogInfo))

  // Laws for Target.Nonsidereal
  checkAll("Eq[Target.Nonsidereal]", EqTests[Target.Nonsidereal].eqv)
  checkAll(
    "Target.Nonsidereal.TrackOrder",
    OrderTests[Target.Nonsidereal](using Target.Nonsidereal.TrackOrder).order
  )
  checkAll(
    "Target.Nonsidereal.NameOrder",
    OrderTests[Target.Nonsidereal](using Target.Nonsidereal.NameOrder).order
  )
  checkAll("Target.Nonsidereal.name", LensTests(Target.Nonsidereal.name))
  checkAll("Target.Nonsidereal.ephemerisKey", LensTests(Target.Nonsidereal.ephemerisKey))
  checkAll("Target.Nonsidereal.sourceProfile", LensTests(Target.Nonsidereal.sourceProfile))
  checkAll(
    "Target.Nonsidereal.integratedSpectralDefinition",
    OptionalTests(Target.Nonsidereal.integratedSpectralDefinition)
  )
  checkAll(
    "Target.Nonsidereal.surfaceSpectralDefinition",
    OptionalTests(Target.Nonsidereal.surfaceSpectralDefinition)
  )
  checkAll("Target.Nonsidereal.fwhm", OptionalTests(Target.Nonsidereal.fwhm))
  checkAll(
    "Target.Nonsidereal.integratedBandNormalizedSpectralDefinition",
    OptionalTests(Target.Nonsidereal.integratedBandNormalizedSpectralDefinition)
  )
  checkAll(
    "Target.Nonsidereal.surfaceBandNormalizedSpectralDefinition",
    OptionalTests(Target.Nonsidereal.surfaceBandNormalizedSpectralDefinition)
  )
  checkAll(
    "Target.Nonsidereal.integratedEmissionLinesSpectralDefinition",
    OptionalTests(Target.Nonsidereal.integratedEmissionLinesSpectralDefinition)
  )
  checkAll(
    "Target.Nonsidereal.surfaceEmissionLinesSpectralDefinition",
    OptionalTests(Target.Nonsidereal.surfaceEmissionLinesSpectralDefinition)
  )
  checkAll(
    "Target.Nonsidereal.unnormalizedSED",
    OptionalTests(Target.Nonsidereal.unnormalizedSED)
  )
  checkAll(
    "Target.Nonsidereal.integratedBrightnesses",
    OptionalTests(Target.Nonsidereal.integratedBrightnesses)
  )
  checkAll(
    "Target.Nonsidereal.surfaceBrightnesses",
    OptionalTests(Target.Nonsidereal.surfaceBrightnesses)
  )
  checkAll(
    "Target.Nonsidereal.integratedBrightnessesT",
    TraversalTests(Target.Nonsidereal.integratedBrightnessesT)
  )
  checkAll(
    "Target.Nonsidereal.surfaceBrightnessesT",
    TraversalTests(Target.Nonsidereal.surfaceBrightnessesT)
  )
  checkAll(
    "Target.Nonsidereal.integratedBrightnessIn",
    TraversalTests(Target.Nonsidereal.integratedBrightnessIn(Band.B))
  )
  checkAll(
    "Target.Nonsidereal.surfaceBrightnessIn",
    TraversalTests(Target.Nonsidereal.surfaceBrightnessIn(Band.B))
  )
  checkAll(
    "Target.Nonsidereal.integratedWavelengthLines",
    OptionalTests(Target.Nonsidereal.integratedWavelengthLines)
  )
  checkAll(
    "Target.Nonsidereal.surfaceWavelengthLines",
    OptionalTests(Target.Nonsidereal.surfaceWavelengthLines)
  )
  checkAll(
    "Target.Nonsidereal.integratedWavelengthLinesT",
    TraversalTests(Target.Nonsidereal.integratedWavelengthLinesT)
  )
  checkAll(
    "Target.Nonsidereal.surfaceWavelengthLinesT",
    TraversalTests(Target.Nonsidereal.surfaceWavelengthLinesT)
  )
  checkAll(
    "Target.Nonsidereal.integratedWavelengthLineIn",
    TraversalTests(
      Target.Nonsidereal.integratedWavelengthLineIn(RedWavelength)
    )
  )
  checkAll(
    "Target.Nonsidereal.surfaceWavelengthLineIn",
    TraversalTests(
      Target.Nonsidereal.surfaceWavelengthLineIn(RedWavelength)
    )
  )
  checkAll(
    "Target.Nonsidereal.integratedFluxDensityContinuum",
    OptionalTests(Target.Nonsidereal.integratedFluxDensityContinuum)
  )
  checkAll(
    "Target.Nonsidereal.surfaceFluxDensityContinuum",
    OptionalTests(Target.Nonsidereal.surfaceFluxDensityContinuum)
  )

  // Laws for Target.Opportunity
  checkAll("Eq[Target.Opportunity]", EqTests[Target.Opportunity].eqv)
  checkAll(
    "Target.Opportunity.RegionOrder", 
    OrderTests[Target.Opportunity](using Target.Opportunity.RegionOrder).order
  )
  checkAll(
    "Target.Opportunity.NameOrder", 
    OrderTests[Target.Opportunity](using Target.Opportunity.NameOrder).order
  )
  checkAll("Target.Opportunity.name", LensTests(Target.Opportunity.name))
  checkAll("Target.Opportunity.region", LensTests(Target.Opportunity.region))
  checkAll("Target.Opportunity.sourceProfile", LensTests(Target.Opportunity.sourceProfile))
  checkAll(
    "Target.Opportunity.integratedSpectralDefinition",
    OptionalTests(Target.Opportunity.integratedSpectralDefinition)
  )
  checkAll(
    "Target.Opportunity.surfaceSpectralDefinition",
    OptionalTests(Target.Opportunity.surfaceSpectralDefinition)
  )
  checkAll("Target.Opportunity.fwhm", OptionalTests(Target.Opportunity.fwhm))
  checkAll(
    "Target.Opportunity.integratedBandNormalizedSpectralDefinition",
    OptionalTests(Target.Opportunity.integratedBandNormalizedSpectralDefinition)
  )
  checkAll(
    "Target.Opportunity.surfaceBandNormalizedSpectralDefinition",
    OptionalTests(Target.Opportunity.surfaceBandNormalizedSpectralDefinition)
  )
  checkAll(
    "Target.Opportunity.integratedEmissionLinesSpectralDefinition",
    OptionalTests(Target.Opportunity.integratedEmissionLinesSpectralDefinition)
  )
  checkAll(
    "Target.Opportunity.surfaceEmissionLinesSpectralDefinition",
    OptionalTests(Target.Opportunity.surfaceEmissionLinesSpectralDefinition)
  )
  checkAll(
    "Target.Opportunity.unnormalizedSED",
    OptionalTests(Target.Opportunity.unnormalizedSED)
  )
  checkAll(
    "Target.Opportunity.integratedBrightnesses",
    OptionalTests(Target.Opportunity.integratedBrightnesses)
  )
  checkAll(
    "Target.Opportunity.surfaceBrightnesses",
    OptionalTests(Target.Opportunity.surfaceBrightnesses)
  )
  checkAll(
    "Target.Opportunity.integratedBrightnessesT",
    TraversalTests(Target.Opportunity.integratedBrightnessesT)
  )
  checkAll(
    "Target.Opportunity.surfaceBrightnessesT",
    TraversalTests(Target.Opportunity.surfaceBrightnessesT)
  )
  checkAll(
    "Target.Opportunity.integratedBrightnessIn",
    TraversalTests(Target.Opportunity.integratedBrightnessIn(Band.B))
  )
  checkAll(
    "Target.Opportunity.surfaceBrightnessIn",
    TraversalTests(Target.Opportunity.surfaceBrightnessIn(Band.B))
  )
  checkAll(
    "Target.Opportunity.integratedWavelengthLines",
    OptionalTests(Target.Opportunity.integratedWavelengthLines)
  )
  checkAll(
    "Target.Opportunity.surfaceWavelengthLines",
    OptionalTests(Target.Opportunity.surfaceWavelengthLines)
  )
  checkAll(
    "Target.Opportunity.integratedWavelengthLinesT",
    TraversalTests(Target.Opportunity.integratedWavelengthLinesT)
  )
  checkAll(
    "Target.Opportunity.surfaceWavelengthLinesT",
    TraversalTests(Target.Opportunity.surfaceWavelengthLinesT)
  )
  checkAll(
    "Target.Opportunity.integratedWavelengthLineIn",
    TraversalTests(
      Target.Opportunity.integratedWavelengthLineIn(RedWavelength)
    )
  )
  checkAll(
    "Target.Opportunity.surfaceWavelengthLineIn",
    TraversalTests(
      Target.Opportunity.surfaceWavelengthLineIn(RedWavelength)
    )
  )
  checkAll(
    "Target.Opportunity.integratedFluxDensityContinuum",
    OptionalTests(Target.Opportunity.integratedFluxDensityContinuum)
  )
  checkAll(
    "Target.Opportunity.surfaceFluxDensityContinuum",
    OptionalTests(Target.Opportunity.surfaceFluxDensityContinuum)
  )


  // Laws for Target
  checkAll("Target.Id", GidTests[Target.Id].gid)
  checkAll("Eq[Target]", EqTests[Target].eqv)
  checkAll("Target.TrackOrder", OrderTests[Target](using Target.TrackOrder).order)
  checkAll("Target.NameOrder", OrderTests[Target](using Target.NameOrder).order)
  checkAll("Target.sidereal", PrismTests(Target.sidereal))
  checkAll("Target.nonsidereal", PrismTests(Target.nonsidereal))
  checkAll("Target.name", LensTests(Target.name))
  checkAll("Target.ephemerisKey", OptionalTests(Target.ephemerisKey))
  checkAll("Target.sourceProfile", LensTests(Target.sourceProfile))
  checkAll(
    "Target.integratedSpectralDefinition",
    OptionalTests(Target.integratedSpectralDefinition)
  )
  checkAll(
    "Target.surfaceSpectralDefinition",
    OptionalTests(Target.surfaceSpectralDefinition)
  )
  checkAll("Target.fwhm", OptionalTests(Target.fwhm))
  checkAll(
    "Target.integratedBandNormalizedSpectralDefinition",
    OptionalTests(Target.integratedBandNormalizedSpectralDefinition)
  )
  checkAll(
    "Target.surfaceBandNormalizedSpectralDefinition",
    OptionalTests(Target.surfaceBandNormalizedSpectralDefinition)
  )
  checkAll(
    "Target.integratedEmissionLinesSpectralDefinition",
    OptionalTests(Target.integratedEmissionLinesSpectralDefinition)
  )
  checkAll(
    "Target.surfaceEmissionLinesSpectralDefinition",
    OptionalTests(Target.surfaceEmissionLinesSpectralDefinition)
  )
  checkAll(
    "Target.unnormalizedSED",
    OptionalTests(Target.unnormalizedSED)
  )
  checkAll(
    "Target.integratedBrightnesses",
    OptionalTests(Target.integratedBrightnesses)
  )
  checkAll(
    "Target.surfaceBrightnesses",
    OptionalTests(Target.surfaceBrightnesses)
  )
  checkAll(
    "Target.integratedBrightnessesT",
    TraversalTests(Target.integratedBrightnessesT)
  )
  checkAll(
    "Target.surfaceBrightnessesT",
    TraversalTests(Target.surfaceBrightnessesT)
  )
  checkAll(
    "Target.integratedBrightnessIn",
    TraversalTests(Target.integratedBrightnessIn(Band.B))
  )
  checkAll(
    "Target.surfaceBrightnessIn",
    TraversalTests(Target.surfaceBrightnessIn(Band.B))
  )
  checkAll(
    "Target.integratedWavelengthLines",
    OptionalTests(Target.integratedWavelengthLines)
  )
  checkAll(
    "Target.surfaceWavelengthLines",
    OptionalTests(Target.surfaceWavelengthLines)
  )
  checkAll(
    "Target.integratedWavelengthLinesT",
    TraversalTests(Target.integratedWavelengthLinesT)
  )
  checkAll(
    "Target.surfaceWavelengthLinesT",
    TraversalTests(Target.surfaceWavelengthLinesT)
  )
  checkAll(
    "Target.integratedWavelengthLineIn",
    TraversalTests(
      Target.integratedWavelengthLineIn(RedWavelength)
    )
  )
  checkAll(
    "Target.surfaceWavelengthLineIn",
    TraversalTests(
      Target.surfaceWavelengthLineIn(RedWavelength)
    )
  )
  checkAll(
    "Target.integratedFluxDensityContinuum",
    OptionalTests(Target.integratedFluxDensityContinuum)
  )
  checkAll(
    "Target.surfaceFluxDensityContinuum",
    OptionalTests(Target.surfaceFluxDensityContinuum)
  )
  checkAll("Target.siderealTracking", OptionalTests(Target.siderealTracking))
  checkAll("Target.parallax", OptionalTests(Target.parallax))
  checkAll("Target.radialVelocity", OptionalTests(Target.radialVelocity))
  checkAll("Target.baseCoordinates", OptionalTests(Target.baseCoordinates))
  checkAll("Target.baseRA", OptionalTests(Target.baseRA))
  checkAll("Target.baseDec", OptionalTests(Target.baseDec))
  checkAll("Target.epoch", OptionalTests(Target.epoch))
  checkAll("Target.properMotion", OptionalTests(Target.properMotion))
  checkAll("Target.properMotionRA", OptionalTests(Target.properMotionRA))
  checkAll("Target.properMotionDec", OptionalTests(Target.properMotionDec))
  checkAll("Target.catalogInfo", OptionalTests(Target.catalogInfo))
}

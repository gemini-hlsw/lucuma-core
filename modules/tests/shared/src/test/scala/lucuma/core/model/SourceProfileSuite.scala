// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import lucuma.core.model.arb._
import monocle.law.discipline._
import munit._
import lucuma.core.util.arb.ArbEnumerated

final class SourceProfileSuite extends DisciplineSuite {
  import ArbSourceProfile._
  import ArbSpectralDefinition._
  import ArbEnumerated._
  // import ArbBandBrightness._
  import ArbGaussianSource._

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
}

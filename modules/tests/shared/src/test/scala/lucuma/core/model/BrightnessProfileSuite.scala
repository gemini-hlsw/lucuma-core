// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import eu.timepit.refined.cats._
// import eu.timepit.refined.scalacheck.string._
// import lucuma.core.arb._
// import lucuma.core.math.arb._
import lucuma.core.model.arb._
// import lucuma.core.util.arb._
// import lucuma.core.util.laws.GidTests
import monocle.law.discipline._
import munit._
// import lucuma.core.enum.Band
// import lucuma.core.math.dimensional._
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
// import org.scalacheck._

final class BrightnessProfileSuite extends DisciplineSuite {
  import ArbBrightnessProfile._
  import ArbSpectralDistribution._
  import ArbEnumerated._
  import ArbTargetBrightness._
  import ArbGaussianSource._

  // Laws for PointBrightnessProfile
  checkAll("Eq[PointBrightnessProfile]", EqTests[PointBrightnessProfile].eqv)
  checkAll("PointBrightnessProfile.brightnesses", LensTests(PointBrightnessProfile.brightnesses))
  checkAll("PointBrightnessProfile.sed", LensTests(PointBrightnessProfile.sed))

  // Laws for UniformBrightnessProfile
  checkAll("Eq[UniformBrightnessProfile]", EqTests[UniformBrightnessProfile].eqv)
  checkAll(
    "UniformBrightnessProfile.brightnesses",
    LensTests(UniformBrightnessProfile.brightnesses)
  )
  checkAll("UniformBrightnessProfile.sed", LensTests(UniformBrightnessProfile.sed))

  // Laws for GaussianBrightnessProfile
  checkAll("Eq[GaussianBrightnessProfile]", EqTests[GaussianBrightnessProfile].eqv)
  checkAll("GaussianBrightnessProfile.source", LensTests(GaussianBrightnessProfile.source))
  checkAll(
    "GaussianBrightnessProfile.brightnesses",
    LensTests(GaussianBrightnessProfile.brightnesses)
  )
  checkAll("GaussianBrightnessProfile.sed", LensTests(GaussianBrightnessProfile.sed))

  // Laws for BrightnessProfile
  checkAll("Eq[BrightnessProfile]", EqTests[BrightnessProfile].eqv)
  checkAll("BrightnessProfile.point", PrismTests(BrightnessProfile.point))
  checkAll("BrightnessProfile.uniform", PrismTests(BrightnessProfile.uniform))
  checkAll("BrightnessProfile.gaussian", PrismTests(BrightnessProfile.gaussian))
  checkAll(
    "BrightnessProfile.integratedBrightnesses",
    OptionalTests(BrightnessProfile.integratedBrightnesses)
  )
  checkAll(
    "BrightnessProfile.surfaceBrightnesses",
    OptionalTests(BrightnessProfile.surfaceBrightnesses)
  )
  checkAll(
    "BrightnessProfile.integratedSED",
    OptionalTests(BrightnessProfile.integratedSED)
  )
  checkAll(
    "BrightnessProfile.surfaceSED",
    OptionalTests(BrightnessProfile.surfaceSED)
  )
  checkAll(
    "BrightnessProfile.gaussianSource",
    OptionalTests(BrightnessProfile.gaussianSource)
  )
}

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.kernel.laws.discipline._
import lucuma.core.model.arb._
import monocle.law.discipline._
import munit._
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.enum.Band

final class BrightnessProfileSuite extends DisciplineSuite {
  import ArbBrightnessProfile._
  import ArbSpectralDistribution._
  import ArbEnumerated._
  import ArbTargetBrightness._
  import ArbGaussianSource._

  // Laws for PointBrightnessProfile
  checkAll("Eq[PointBrightnessProfile]", EqTests[PointBrightnessProfile].eqv)
  checkAll("PointBrightnessProfile.brightnesses", LensTests(PointBrightnessProfile.brightnesses))
  checkAll(
    "PointBrightnessProfile.brightnessesT",
    TraversalTests(PointBrightnessProfile.brightnessesT)
  )
  checkAll(
    "PointBrightnessProfile.brightnessIn",
    TraversalTests(PointBrightnessProfile.brightnessIn(Band.B))
  )
  checkAll("PointBrightnessProfile.sed", LensTests(PointBrightnessProfile.sed))

  // Laws for UniformBrightnessProfile
  checkAll("Eq[UniformBrightnessProfile]", EqTests[UniformBrightnessProfile].eqv)
  checkAll(
    "UniformBrightnessProfile.brightnesses",
    LensTests(UniformBrightnessProfile.brightnesses)
  )
  checkAll(
    "UniformBrightnessProfile.brightnessesT",
    TraversalTests(UniformBrightnessProfile.brightnessesT)
  )
  checkAll(
    "UniformBrightnessProfile.brightnessIn",
    TraversalTests(UniformBrightnessProfile.brightnessIn(Band.B))
  )
  checkAll("UniformBrightnessProfile.sed", LensTests(UniformBrightnessProfile.sed))

  // Laws for GaussianBrightnessProfile
  checkAll("Eq[GaussianBrightnessProfile]", EqTests[GaussianBrightnessProfile].eqv)
  checkAll("GaussianBrightnessProfile.source", LensTests(GaussianBrightnessProfile.source))
  checkAll(
    "GaussianBrightnessProfile.brightnesses",
    LensTests(GaussianBrightnessProfile.brightnesses)
  )
  checkAll(
    "GaussianBrightnessProfile.brightnessesT",
    TraversalTests(GaussianBrightnessProfile.brightnessesT)
  )
  checkAll(
    "GaussianBrightnessProfile.brightnessIn",
    TraversalTests(GaussianBrightnessProfile.brightnessIn(Band.B))
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
    "BrightnessProfile.integratedBrightnessesT",
    TraversalTests(BrightnessProfile.integratedBrightnessesT)
  )
  checkAll(
    "BrightnessProfile.integratedBrightnessIn",
    TraversalTests(BrightnessProfile.integratedBrightnessIn(Band.B))
  )
  checkAll(
    "BrightnessProfile.surfaceBrightnesses",
    OptionalTests(BrightnessProfile.surfaceBrightnesses)
  )
  checkAll(
    "BrightnessProfile.surfaceBrightnessesT",
    TraversalTests(BrightnessProfile.surfaceBrightnessesT)
  )
  checkAll(
    "BrightnessProfile.surfaceBrightnessIn",
    TraversalTests(BrightnessProfile.surfaceBrightnessIn(Band.B))
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

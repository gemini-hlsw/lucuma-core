// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.effect.IO
import cats.effect.Resource
import lucuma.catalog.simbad.SEDDataConfig
import lucuma.catalog.simbad.SEDDataLoader
import lucuma.catalog.simbad.SEDMatcher
import lucuma.catalog.simbad.StellarLibraryParameters
import lucuma.catalog.simbad.StellarPhysics
import munit.CatsEffectSuite

case class SEDFixtureData(
  config:  SEDDataConfig,
  physics: StellarPhysics,
  library: StellarLibraryParameters,
  matcher: SEDMatcher
)

trait SEDMatcherFixture:
  self: CatsEffectSuite =>

  val sedFixture = ResourceSuiteLocalFixture(
    "sedFixture",
    Resource.eval(SEDDataLoader.load[IO].map { config =>
      val physics = new StellarPhysics(config.gravityTable)
      val library = new StellarLibraryParameters(config.stellarLibrary, physics)
      val matcher = new SEDMatcher(library, physics)
      SEDFixtureData(config, physics, library, matcher)
    })
  )

  def sedConfig: SEDDataConfig             = sedFixture().config
  def sedPhysics: StellarPhysics           = sedFixture().physics
  def sedLibrary: StellarLibraryParameters = sedFixture().library
  def sedMatcher: SEDMatcher               = sedFixture().matcher

  val testData: IO[List[SimbadEntry]]          = SimbadData.testData
  val expectedOutput: IO[List[ExpectedOutput]] = SimbadData.expectedOutput

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.effect.Resource
import lucuma.catalog.simbad.SEDDataConfig
import lucuma.catalog.simbad.SEDDataLoader
import lucuma.catalog.simbad.SEDMatcher
import munit.CatsEffectSuite

trait SEDMatcherFixture:
  self: CatsEffectSuite =>

  val sedMatcherFixture = ResourceSuiteLocalFixture(
    "sedMatcher",
    Resource.eval(SEDDataLoader.load.map(SEDMatcher.fromConfig))
  )

  val sedConfigFixture = ResourceSuiteLocalFixture(
    "sedConfig",
    Resource.eval(SEDDataLoader.load)
  )

  def sedMatcher: SEDMatcher   = sedMatcherFixture()
  def sedConfig: SEDDataConfig = sedConfigFixture()

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.model.arb.ArbConfiguration.given
import munit.ScalaCheckSuite
import org.scalacheck.*
import org.scalacheck.Prop.*

final class ConfigurationSuite extends ScalaCheckSuite:
  import Configuration.ObservingMode.*

  test("GMOS North Imaging has no constraints"):
    forAll: (cfg: Configuration, a: GmosNorthImaging, b: GmosNorthImaging) =>
      val ca = cfg.copy(observingMode = a)
      val cb = cfg.copy(observingMode = b)
      assert(ca.subsumes(cb))
      assert(cb.subsumes(ca))

  test("GMOS South Imaging has no constraints"):
    forAll: (cfg: Configuration, a: GmosSouthImaging, b: GmosSouthImaging) =>
      val ca = cfg.copy(observingMode = a)
      val cb = cfg.copy(observingMode = b)
      assert(ca.subsumes(cb))
      assert(cb.subsumes(ca))

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.model.sequence.gmos.arb.ArbDynamicConfig
import lucuma.core.model.sequence.gmos.arb.ArbStaticConfig
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbInstrumentExecutionConfig {

  import ArbDynamicConfig._
  import ArbExecutionConfig.given
  import ArbStaticConfig._

  given Arbitrary[InstrumentExecutionConfig.GmosNorth] =
    Arbitrary {
      arbitrary[ExecutionConfig[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]].map(InstrumentExecutionConfig.GmosNorth(_))
    }

  given Arbitrary[InstrumentExecutionConfig.GmosSouth] =
    Arbitrary {
      arbitrary[ExecutionConfig[StaticConfig.GmosSouth, DynamicConfig.GmosSouth]].map(InstrumentExecutionConfig.GmosSouth(_))
    }

  given Arbitrary[InstrumentExecutionConfig] =
    Arbitrary {
      Gen.oneOf(arbitrary[InstrumentExecutionConfig.GmosNorth], arbitrary[InstrumentExecutionConfig.GmosSouth])
    }

  given Cogen[InstrumentExecutionConfig.GmosNorth] =
    Cogen[ExecutionConfig[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]].contramap(_.executionConfig)

  given Cogen[InstrumentExecutionConfig.GmosSouth] =
    Cogen[ExecutionConfig[StaticConfig.GmosSouth, DynamicConfig.GmosSouth]].contramap(_.executionConfig)

  given Cogen[InstrumentExecutionConfig] =
    Cogen[(
      Option[InstrumentExecutionConfig.GmosNorth],
      Option[InstrumentExecutionConfig.GmosSouth]
    )].contramap { a => (
      InstrumentExecutionConfig.gmosNorth.getOption(a),
      InstrumentExecutionConfig.gmosSouth.getOption(a)
    )}
}

object ArbInstrumentExecutionConfig extends ArbInstrumentExecutionConfig

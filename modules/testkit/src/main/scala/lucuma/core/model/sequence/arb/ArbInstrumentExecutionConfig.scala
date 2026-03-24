// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.model.sequence.flamingos2.arb.ArbFlamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.arb.ArbFlamingos2StaticConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.model.sequence.gmos.arb.ArbDynamicConfig
import lucuma.core.model.sequence.gmos.arb.ArbStaticConfig
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.core.model.sequence.igrins2.arb.ArbIgrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.arb.ArbIgrins2StaticConfig
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbInstrumentExecutionConfig:

  import ArbDynamicConfig.given
  import ArbExecutionConfig.given
  import ArbFlamingos2DynamicConfig.given
  import ArbFlamingos2StaticConfig.given
  import ArbIgrins2DynamicConfig.given
  import ArbIgrins2StaticConfig.given
  import ArbStaticConfig.given

  given Arbitrary[InstrumentExecutionConfig.Flamingos2] =
    Arbitrary:
      arbitrary[ExecutionConfig[Flamingos2StaticConfig, Flamingos2DynamicConfig]].map(InstrumentExecutionConfig.Flamingos2(_))

  given Arbitrary[InstrumentExecutionConfig.GmosNorth] =
    Arbitrary:
      arbitrary[ExecutionConfig[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]].map(InstrumentExecutionConfig.GmosNorth(_))

  given Arbitrary[InstrumentExecutionConfig.GmosSouth] =
    Arbitrary:
      arbitrary[ExecutionConfig[StaticConfig.GmosSouth, DynamicConfig.GmosSouth]].map(InstrumentExecutionConfig.GmosSouth(_))

  given Arbitrary[InstrumentExecutionConfig.Igrins2] =
    Arbitrary:
      arbitrary[ExecutionConfig[Igrins2StaticConfig, Igrins2DynamicConfig]].map(InstrumentExecutionConfig.Igrins2(_))

  given Arbitrary[InstrumentExecutionConfig] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[InstrumentExecutionConfig.Flamingos2],
        arbitrary[InstrumentExecutionConfig.GmosNorth],
        arbitrary[InstrumentExecutionConfig.GmosSouth],
        arbitrary[InstrumentExecutionConfig.Igrins2]
      )

  given Cogen[InstrumentExecutionConfig.Flamingos2] =
    Cogen[ExecutionConfig[Flamingos2StaticConfig, Flamingos2DynamicConfig]].contramap(_.executionConfig)

  given Cogen[InstrumentExecutionConfig.GmosNorth] =
    Cogen[ExecutionConfig[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]].contramap(_.executionConfig)

  given Cogen[InstrumentExecutionConfig.GmosSouth] =
    Cogen[ExecutionConfig[StaticConfig.GmosSouth, DynamicConfig.GmosSouth]].contramap(_.executionConfig)

  given Cogen[InstrumentExecutionConfig.Igrins2] =
    Cogen[ExecutionConfig[Igrins2StaticConfig, Igrins2DynamicConfig]].contramap(_.executionConfig)

  given Cogen[InstrumentExecutionConfig] =
    Cogen[(
      Option[InstrumentExecutionConfig.Flamingos2],
      Option[InstrumentExecutionConfig.GmosNorth],
      Option[InstrumentExecutionConfig.GmosSouth],
      Option[InstrumentExecutionConfig.Igrins2]
    )].contramap { a => (
      InstrumentExecutionConfig.flamingos2.getOption(a),
      InstrumentExecutionConfig.gmosNorth.getOption(a),
      InstrumentExecutionConfig.gmosSouth.getOption(a),
      InstrumentExecutionConfig.igrins2.getOption(a)
    )}

object ArbInstrumentExecutionConfig extends ArbInstrumentExecutionConfig

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package arb

import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig
import lucuma.core.model.sequence.flamingos2.arb.ArbFlamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.arb.ArbFlamingos2StaticConfig
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import lucuma.core.model.sequence.ghost.arb.ArbGhostDynamicConfig
import lucuma.core.model.sequence.ghost.arb.ArbGhostStaticConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.StaticConfig
import lucuma.core.model.sequence.gmos.arb.ArbDynamicConfig
import lucuma.core.model.sequence.gmos.arb.ArbStaticConfig
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.model.sequence.gnirs.arb.ArbGnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.arb.ArbGnirsStaticConfig
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.core.model.sequence.igrins2.arb.ArbIgrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.arb.ArbIgrins2StaticConfig
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbInstrumentExecutionConfig:

  import ArbDynamicConfig.given
  import ArbEnumerated.given
  import ArbExecutionConfig.given
  import ArbFlamingos2DynamicConfig.given
  import ArbFlamingos2StaticConfig.given
  import ArbGhostDynamicConfig.given
  import ArbGhostStaticConfig.given
  import ArbGnirsDynamicConfig.given
  import ArbGnirsStaticConfig.given
  import ArbIgrins2DynamicConfig.given
  import ArbIgrins2StaticConfig.given
  import ArbStaticConfig.given

  given Arbitrary[InstrumentExecutionConfig.Exchange.type] =
    Arbitrary:
      Gen.const(InstrumentExecutionConfig.Exchange)

  given Arbitrary[InstrumentExecutionConfig.Flamingos2] =
    Arbitrary:
      arbitrary[ExecutionConfig[Flamingos2StaticConfig, Flamingos2DynamicConfig]].map(InstrumentExecutionConfig.Flamingos2(_))

  given Arbitrary[InstrumentExecutionConfig.Ghost] =
    Arbitrary:
      arbitrary[ExecutionConfig[GhostStaticConfig, GhostDynamicConfig]].map(InstrumentExecutionConfig.Ghost(_))

  given Arbitrary[InstrumentExecutionConfig.GmosNorth] =
    Arbitrary:
      arbitrary[ExecutionConfig[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]].map(InstrumentExecutionConfig.GmosNorth(_))

  given Arbitrary[InstrumentExecutionConfig.GmosSouth] =
    Arbitrary:
      arbitrary[ExecutionConfig[StaticConfig.GmosSouth, DynamicConfig.GmosSouth]].map(InstrumentExecutionConfig.GmosSouth(_))

  given Arbitrary[InstrumentExecutionConfig.Gnirs] =
    Arbitrary:
      arbitrary[ExecutionConfig[GnirsStaticConfig, GnirsDynamicConfig]].map(InstrumentExecutionConfig.Gnirs(_))

  given Arbitrary[InstrumentExecutionConfig.Igrins2] =
    Arbitrary:
      arbitrary[ExecutionConfig[Igrins2StaticConfig, Igrins2DynamicConfig]].map(InstrumentExecutionConfig.Igrins2(_))

  given Arbitrary[InstrumentExecutionConfig.Visitor] =
    Arbitrary:
      Gen
        .oneOf(Instrument.visitorInstruments)
        .map(InstrumentExecutionConfig.Visitor(_))

  given Arbitrary[InstrumentExecutionConfig] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[InstrumentExecutionConfig.Exchange.type],
        arbitrary[InstrumentExecutionConfig.Flamingos2],
        arbitrary[InstrumentExecutionConfig.Ghost],
        arbitrary[InstrumentExecutionConfig.GmosNorth],
        arbitrary[InstrumentExecutionConfig.GmosSouth],
        arbitrary[InstrumentExecutionConfig.Gnirs],
        arbitrary[InstrumentExecutionConfig.Igrins2],
        arbitrary[InstrumentExecutionConfig.Visitor]
      )

  given Cogen[InstrumentExecutionConfig.Exchange.type] =
    Cogen[Unit].contramap(_ => ())

  given Cogen[InstrumentExecutionConfig.Flamingos2] =
    Cogen[ExecutionConfig[Flamingos2StaticConfig, Flamingos2DynamicConfig]].contramap(_.executionConfig)

  given Cogen[InstrumentExecutionConfig.Ghost] =
    Cogen[ExecutionConfig[GhostStaticConfig, GhostDynamicConfig]].contramap(_.executionConfig)

  given Cogen[InstrumentExecutionConfig.GmosNorth] =
    Cogen[ExecutionConfig[StaticConfig.GmosNorth, DynamicConfig.GmosNorth]].contramap(_.executionConfig)

  given Cogen[InstrumentExecutionConfig.GmosSouth] =
    Cogen[ExecutionConfig[StaticConfig.GmosSouth, DynamicConfig.GmosSouth]].contramap(_.executionConfig)

  given Cogen[InstrumentExecutionConfig.Gnirs] =
    Cogen[ExecutionConfig[GnirsStaticConfig, GnirsDynamicConfig]].contramap(_.executionConfig)

  given Cogen[InstrumentExecutionConfig.Igrins2] =
    Cogen[ExecutionConfig[Igrins2StaticConfig, Igrins2DynamicConfig]].contramap(_.executionConfig)

  given Cogen[InstrumentExecutionConfig.Visitor] =
    Cogen[Instrument]
      .contramap: v =>
        v.visitorInstrument

  given Cogen[InstrumentExecutionConfig] =
    Cogen[(
      Option[InstrumentExecutionConfig.Exchange.type],
      Option[InstrumentExecutionConfig.Flamingos2],
      Option[InstrumentExecutionConfig.Ghost],
      Option[InstrumentExecutionConfig.GmosNorth],
      Option[InstrumentExecutionConfig.GmosSouth],
      Option[InstrumentExecutionConfig.Gnirs],
      Option[InstrumentExecutionConfig.Igrins2],
      Option[InstrumentExecutionConfig.Visitor]
    )].contramap { a => (
      InstrumentExecutionConfig.exchange.getOption(a),
      InstrumentExecutionConfig.flamingos2.getOption(a),
      InstrumentExecutionConfig.ghost.getOption(a),
      InstrumentExecutionConfig.gmosNorth.getOption(a),
      InstrumentExecutionConfig.gmosSouth.getOption(a),
      InstrumentExecutionConfig.gnirs.getOption(a),
      InstrumentExecutionConfig.igrins2.getOption(a),
      InstrumentExecutionConfig.visitor.getOption(a)
    )}

object ArbInstrumentExecutionConfig extends ArbInstrumentExecutionConfig

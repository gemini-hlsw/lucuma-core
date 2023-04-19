// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.syntax.eq.*
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.gmos
import monocle.Prism
import monocle.macros.GenPrism

/**
 * `ExecutionConfig` instances for each instrument.
 */
sealed trait InstrumentExecutionConfig {
  def instrument: Instrument
}

object InstrumentExecutionConfig {

  case class GmosNorth(
    executionConfig: ExecutionConfig[gmos.StaticConfig.GmosNorth, gmos.DynamicConfig.GmosNorth]
  ) extends InstrumentExecutionConfig {
    def instrument: Instrument =
      Instrument.GmosNorth
  }

  object GmosNorth {
    given Eq[GmosNorth] =
      Eq.by(_.executionConfig)
  }

  val gmosNorth: Prism[InstrumentExecutionConfig, GmosNorth] =
    GenPrism[InstrumentExecutionConfig, GmosNorth]

  case class GmosSouth(
    executionConfig: ExecutionConfig[gmos.StaticConfig.GmosSouth, gmos.DynamicConfig.GmosSouth]
  ) extends InstrumentExecutionConfig {
    def instrument: Instrument =
      Instrument.GmosSouth
  }

  object GmosSouth {
    given Eq[GmosSouth] =
      Eq.by(_.executionConfig)
  }

  val gmosSouth: Prism[InstrumentExecutionConfig, GmosSouth] =
    GenPrism[InstrumentExecutionConfig, GmosSouth]

  given Eq[InstrumentExecutionConfig] =
    Eq.instance {
      case (a @ GmosNorth(_), b @ GmosNorth(_)) => a === b
      case (a @ GmosSouth(_), b @ GmosSouth(_)) => a === b
      case _                                    => false
    }

}
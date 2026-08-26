// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.derived.*
import cats.syntax.eq.*
import cats.syntax.option.*
import lucuma.core.enums.Instrument
import lucuma.core.model.sequence.flamingos2 as f2
import lucuma.core.model.sequence.ghost.GhostDynamicConfig
import lucuma.core.model.sequence.ghost.GhostStaticConfig
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.model.sequence.igrins2 as ig2
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

/**
 * `ExecutionConfig` instances for each instrument.
 */
sealed trait InstrumentExecutionConfig:

  /**
   * Returns the instrument discriminator associated with this execution config,
   * if any.  Exchange observations are not Gemini instruments and have none.
   */
  def instrument: Option[Instrument]

  /** Returns `true` if there are no science steps to execute. */
  def isComplete: Boolean

object InstrumentExecutionConfig:

  /**
   * Exchange observations (Keck/Subaru) have no Gemini instrument and no
   * generated sequence.
   */
  case object Exchange extends InstrumentExecutionConfig:
    override def instrument: Option[Instrument] = none
    override def isComplete: Boolean            = true

  val exchange: Prism[InstrumentExecutionConfig, Exchange.type] =
    GenPrism[InstrumentExecutionConfig, Exchange.type]

  case class Flamingos2(
    executionConfig: ExecutionConfig[f2.Flamingos2StaticConfig, f2.Flamingos2DynamicConfig]
  ) extends InstrumentExecutionConfig:
    override def instrument: Option[Instrument] = Instrument.Flamingos2.some
    override def isComplete: Boolean    = executionConfig.isComplete

  object Flamingos2:
    given Eq[Flamingos2] =
      Eq.by(_.executionConfig)

    val executionConfig
      : Lens[Flamingos2, ExecutionConfig[f2.Flamingos2StaticConfig, f2.Flamingos2DynamicConfig]] =
      Focus[Flamingos2](_.executionConfig)

  val flamingos2: Prism[InstrumentExecutionConfig, Flamingos2] =
    GenPrism[InstrumentExecutionConfig, Flamingos2]

  case class Ghost(
    executionConfig: ExecutionConfig[GhostStaticConfig, GhostDynamicConfig]
  ) extends InstrumentExecutionConfig derives Eq:
    override def instrument: Option[Instrument] = Instrument.Ghost.some
    override def isComplete: Boolean    = executionConfig.isComplete

  object Ghost:
    val executionConfig: Lens[Ghost, ExecutionConfig[GhostStaticConfig, GhostDynamicConfig]] =
      Focus[Ghost](_.executionConfig)

  val ghost: Prism[InstrumentExecutionConfig, Ghost] =
    GenPrism[InstrumentExecutionConfig, Ghost]

  case class GmosNorth(
    executionConfig: ExecutionConfig[gmos.StaticConfig.GmosNorth, gmos.DynamicConfig.GmosNorth]
  ) extends InstrumentExecutionConfig:
    override def instrument: Option[Instrument] = Instrument.GmosNorth.some
    override def isComplete: Boolean    = executionConfig.isComplete

  object GmosNorth:
    given Eq[GmosNorth] =
      Eq.by(_.executionConfig)

    val executionConfig: Lens[GmosNorth, ExecutionConfig[gmos.StaticConfig.GmosNorth,
                                                         gmos.DynamicConfig.GmosNorth
    ]] =
      Focus[GmosNorth](_.executionConfig)

  val gmosNorth: Prism[InstrumentExecutionConfig, GmosNorth] =
    GenPrism[InstrumentExecutionConfig, GmosNorth]

  case class GmosSouth(
    executionConfig: ExecutionConfig[gmos.StaticConfig.GmosSouth, gmos.DynamicConfig.GmosSouth]
  ) extends InstrumentExecutionConfig:
    override def instrument: Option[Instrument] = Instrument.GmosSouth.some
    override def isComplete: Boolean    = executionConfig.isComplete

  object GmosSouth:
    given Eq[GmosSouth] =
      Eq.by(_.executionConfig)

    val executionConfig: Lens[GmosSouth, ExecutionConfig[gmos.StaticConfig.GmosSouth,
                                                         gmos.DynamicConfig.GmosSouth
    ]] =
      Focus[GmosSouth](_.executionConfig)

  val gmosSouth: Prism[InstrumentExecutionConfig, GmosSouth] =
    GenPrism[InstrumentExecutionConfig, GmosSouth]

  case class Gnirs(
    executionConfig: ExecutionConfig[GnirsStaticConfig, GnirsDynamicConfig]
  ) extends InstrumentExecutionConfig:
    override def instrument: Option[Instrument] = Instrument.Gnirs.some
    override def isComplete: Boolean    = executionConfig.isComplete

  object Gnirs:
    given Eq[Gnirs] =
      Eq.by(_.executionConfig)

    val executionConfig
      : Lens[Gnirs, ExecutionConfig[GnirsStaticConfig, GnirsDynamicConfig]] =
      Focus[Gnirs](_.executionConfig)

  val gnirs: Prism[InstrumentExecutionConfig, Gnirs] =
    GenPrism[InstrumentExecutionConfig, Gnirs]

  case class Igrins2(
    executionConfig: ExecutionConfig[ig2.Igrins2StaticConfig, ig2.Igrins2DynamicConfig]
  ) extends InstrumentExecutionConfig derives Eq:
    override def instrument: Option[Instrument] = Instrument.Igrins2.some
    override def isComplete: Boolean    = executionConfig.isComplete

  object Igrins2:
    val executionConfig : Lens[Igrins2, ExecutionConfig[ig2.Igrins2StaticConfig, ig2.Igrins2DynamicConfig]] =
      Focus[Igrins2](_.executionConfig)

  val igrins2: Prism[InstrumentExecutionConfig, Igrins2] =
    GenPrism[InstrumentExecutionConfig, Igrins2]

  case class Visitor(visitorInstrument: Instrument) extends InstrumentExecutionConfig derives Eq:
    override def instrument: Option[Instrument] = visitorInstrument.some
    val isComplete = true

  val visitor: Prism[InstrumentExecutionConfig, Visitor] =
    GenPrism[InstrumentExecutionConfig, Visitor]

  given Eq[InstrumentExecutionConfig] =
    Eq.instance:
      case (Exchange, Exchange)                   => true
      case (a @ Flamingos2(_), b @ Flamingos2(_)) => a === b
      case (a @ Ghost(_),      b @ Ghost(_))      => a === b
      case (a @ GmosNorth(_),  b @ GmosNorth(_))  => a === b
      case (a @ GmosSouth(_),  b @ GmosSouth(_))  => a === b
      case (a @ Gnirs(_),      b @ Gnirs(_))      => a === b
      case (a @ Igrins2(_),    b @ Igrins2(_))    => a === b
      case (a @ Visitor(_),    b @ Visitor(_))    => a === b
      case _                                      => false
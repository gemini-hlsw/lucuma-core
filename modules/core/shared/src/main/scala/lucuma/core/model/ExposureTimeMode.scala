// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.syntax.eq._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.model.NonNegDuration
import lucuma.core.model.implicits._
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

sealed trait ExposureTimeMode extends Product with Serializable

object ExposureTimeMode {

  final case class SignalToNoise(value: PosBigDecimal)                   extends ExposureTimeMode
  final case class FixedExposure(count: NonNegInt, time: NonNegDuration) extends ExposureTimeMode

  implicit val EqExposureMode: Eq[ExposureTimeMode] =
    Eq.instance {
      case (SignalToNoise(a), SignalToNoise(b))           => a === b
      case (FixedExposure(ac, ad), FixedExposure(bc, bd)) => ac === bc && ad === bd
      case _                                              => false
    }

  val signalToNoise: Prism[ExposureTimeMode, SignalToNoise] =
    GenPrism[ExposureTimeMode, SignalToNoise]

  val fixedExposure: Prism[ExposureTimeMode, FixedExposure] =
    GenPrism[ExposureTimeMode, FixedExposure]

  object SignalToNoise {
    val value: Lens[SignalToNoise, PosBigDecimal] = Focus[SignalToNoise](_.value)

    implicit val eqSignalToNoise: Eq[SignalToNoise] = Eq.by(_.value)
  }

  object FixedExposure {
    val count: Lens[FixedExposure, NonNegInt]     = Focus[FixedExposure](_.count)
    val time: Lens[FixedExposure, NonNegDuration] = Focus[FixedExposure](_.time)

    implicit val eqFixedExposure: Eq[FixedExposure] = Eq.by(f => (f.count, f.time))
  }
}

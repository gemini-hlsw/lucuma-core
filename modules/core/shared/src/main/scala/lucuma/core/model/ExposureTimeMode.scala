// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.math.SignalToNoise
import lucuma.core.model.NonNegDuration
import lucuma.core.model.given
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.macros.GenPrism

sealed trait ExposureTimeMode extends Product with Serializable

object ExposureTimeMode {

  final case class SignalToNoiseMode(value: SignalToNoise)             extends ExposureTimeMode
  final case class FixedExposureMode(count: NonNegInt, time: TimeSpan) extends ExposureTimeMode

  given Eq[ExposureTimeMode] =
    Eq.instance {
      case (SignalToNoiseMode(a), SignalToNoiseMode(b))           => a === b
      case (FixedExposureMode(ac, ad), FixedExposureMode(bc, bd)) => ac === bc && ad === bd
      case _                                              => false
    }

  val signalToNoise: Prism[ExposureTimeMode, SignalToNoiseMode] =
    GenPrism[ExposureTimeMode, SignalToNoiseMode]

  val fixedExposure: Prism[ExposureTimeMode, FixedExposureMode] =
    GenPrism[ExposureTimeMode, FixedExposureMode]

  val signalToNoiseValue: Optional[ExposureTimeMode, SignalToNoise] =
    Optional[ExposureTimeMode, SignalToNoise] {
      case SignalToNoiseMode(value) => value.some
      case FixedExposureMode(_, _)  => none
    } { nnd =>
      {
        case s @ SignalToNoiseMode(_)    => SignalToNoiseMode.value.replace(nnd)(s)
        case f @ FixedExposureMode(_, _) => f
      }
    }

  val exposureCount: Optional[ExposureTimeMode, NonNegInt] =
    Optional[ExposureTimeMode, NonNegInt] {
      case FixedExposureMode(count, _) => count.some
      case SignalToNoiseMode(_)        => none
    } { nni =>
      {
        case f @ FixedExposureMode(_, _) => FixedExposureMode.count.replace(nni)(f)
        case s @ SignalToNoiseMode(_)    => s
      }
    }

  val exposureTime: Optional[ExposureTimeMode, TimeSpan] =
    Optional[ExposureTimeMode, TimeSpan] {
      case FixedExposureMode(_, time) => time.some
      case SignalToNoiseMode(_)       => none
    } { pbd =>
      {
        case f @ FixedExposureMode(_, _) => FixedExposureMode.time.replace(pbd)(f)
        case s @ SignalToNoiseMode(_)    => s
      }
    }

  object SignalToNoiseMode {
    val value: Lens[SignalToNoiseMode, SignalToNoise] = Focus[SignalToNoiseMode](_.value)

    given Eq[SignalToNoiseMode] = Eq.by(_.value)
  }

  object FixedExposureMode {
    val count: Lens[FixedExposureMode, NonNegInt] = Focus[FixedExposureMode](_.count)
    val time: Lens[FixedExposureMode, TimeSpan]   = Focus[FixedExposureMode](_.time)

    given Eq[FixedExposureMode] = Eq.by(f => (f.count, f.time))
  }
}

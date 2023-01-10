// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosBigDecimal
import lucuma.core.model.NonNegDuration
import lucuma.core.model.given
import monocle.Focus
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.macros.GenPrism

sealed trait ExposureTimeMode extends Product with Serializable

object ExposureTimeMode {

  final case class SignalToNoise(value: PosBigDecimal)                   extends ExposureTimeMode
  final case class FixedExposure(count: NonNegInt, time: NonNegDuration) extends ExposureTimeMode

  given Eq[ExposureTimeMode] =
    Eq.instance {
      case (SignalToNoise(a), SignalToNoise(b))           => a === b
      case (FixedExposure(ac, ad), FixedExposure(bc, bd)) => ac === bc && ad === bd
      case _                                              => false
    }

  val signalToNoise: Prism[ExposureTimeMode, SignalToNoise] =
    GenPrism[ExposureTimeMode, SignalToNoise]

  val fixedExposure: Prism[ExposureTimeMode, FixedExposure] =
    GenPrism[ExposureTimeMode, FixedExposure]

  val signalToNoiseValue: Optional[ExposureTimeMode, PosBigDecimal] =
    Optional[ExposureTimeMode, PosBigDecimal] {
      case SignalToNoise(value) => value.some
      case FixedExposure(_, _)  => none
    } { nnd =>
      {
        case s @ SignalToNoise(_)    => SignalToNoise.value.replace(nnd)(s)
        case f @ FixedExposure(_, _) => f
      }
    }

  val exposureCount: Optional[ExposureTimeMode, NonNegInt] =
    Optional[ExposureTimeMode, NonNegInt] {
      case FixedExposure(count, _) => count.some
      case SignalToNoise(_)        => none
    } { nni =>
      {
        case f @ FixedExposure(_, _) => FixedExposure.count.replace(nni)(f)
        case s @ SignalToNoise(_)    => s
      }
    }

  val exposureTime: Optional[ExposureTimeMode, NonNegDuration] =
    Optional[ExposureTimeMode, NonNegDuration] {
      case FixedExposure(_, time) => time.some
      case SignalToNoise(_)       => none
    } { pbd =>
      {
        case f @ FixedExposure(_, _) => FixedExposure.time.replace(pbd)(f)
        case s @ SignalToNoise(_)    => s
      }
    }

  object SignalToNoise {
    val value: Lens[SignalToNoise, PosBigDecimal] = Focus[SignalToNoise](_.value)

    given Eq[SignalToNoise] = Eq.by(_.value)
  }

  object FixedExposure {
    val count: Lens[FixedExposure, NonNegInt]     = Focus[FixedExposure](_.count)
    val time: Lens[FixedExposure, NonNegDuration] = Focus[FixedExposure](_.time)

    given Eq[FixedExposure] = Eq.by(f => (f.count, f.time))
  }
}

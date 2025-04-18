// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

sealed trait ExposureTimeMode extends Product with Serializable:
  def at:  Wavelength

object ExposureTimeMode:

  final case class SignalToNoiseMode(value: SignalToNoise, at: Wavelength) extends ExposureTimeMode

  object SignalToNoiseMode:
    val value: Lens[SignalToNoiseMode, SignalToNoise] = Focus[SignalToNoiseMode](_.value)
    val at:    Lens[SignalToNoiseMode, Wavelength]    = Focus[SignalToNoiseMode](_.at)
    given Eq[SignalToNoiseMode]                       = Eq.by(a => (a.value, a.at))


  final case class TimeAndCountMode(time: TimeSpan, count: NonNegInt, at: Wavelength) extends ExposureTimeMode

  object TimeAndCountMode:
    val time:  Lens[TimeAndCountMode, TimeSpan]   = Focus[TimeAndCountMode](_.time)
    val count: Lens[TimeAndCountMode, NonNegInt]  = Focus[TimeAndCountMode](_.count)
    val at:    Lens[TimeAndCountMode, Wavelength] = Focus[TimeAndCountMode](_.at)
    given Eq[TimeAndCountMode]                    = Eq.by(a => (a.time, a.count, a.at))


  given Eq[ExposureTimeMode] =
    Eq.instance:
      case (a@SignalToNoiseMode(_, _),    b@SignalToNoiseMode(_, _))   => a === b
      case (a@TimeAndCountMode(_, _, _),  b@TimeAndCountMode(_, _, _)) => a === b
      case _                                                           => false

  val signalToNoise: Prism[ExposureTimeMode, SignalToNoiseMode] =
    GenPrism[ExposureTimeMode, SignalToNoiseMode]

  val timeAndCount: Prism[ExposureTimeMode, TimeAndCountMode] =
    GenPrism[ExposureTimeMode, TimeAndCountMode]

  val at: Lens[ExposureTimeMode, Wavelength] =
    Lens[ExposureTimeMode, Wavelength] {
      case SignalToNoiseMode(_, w)   => w
      case TimeAndCountMode(_, _, w) => w
    } { w =>
      {
        case a @ SignalToNoiseMode(_, _)   => SignalToNoiseMode.at.replace(w)(a)
        case a @ TimeAndCountMode(_, _, _) => TimeAndCountMode.at.replace(w)(a)
      }
    }
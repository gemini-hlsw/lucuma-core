// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.derived.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.ScienceMode
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

import scala.reflect.TypeTest

case class WavelengthW(w: Wavelength)
type AtForMode[M <: ScienceMode] = M match {
  case ScienceMode.Spectroscopy.type => Wavelength
  case ScienceMode.Imaging.type => Unit
}

extension[M <: ScienceMode](at: AtForMode[M])
  def toEither(using tt: TypeTest[Any, Wavelength]): Either[Wavelength, Unit] =
    at match
      case tt(w: Wavelength) => Left(w)
      case _                 => Right(())

  def toOption(using tt: TypeTest[Any, Wavelength]): Option[Wavelength] =
    at match
      case w @ Wavelength(_) => Some(w)
      case _                 => None

sealed trait ExposureTimeMode[M <: ScienceMode]:
  def at: AtForMode[M]

object ExposureTimeMode:
  final case class SignalToNoiseMode[M <: ScienceMode] private (value: SignalToNoise, at: AtForMode[M]) extends ExposureTimeMode[M]
  final case class TimeAndCountMode[M <: ScienceMode] private (time: TimeSpan, count: PosInt, at: AtForMode[M]) extends ExposureTimeMode[M]

  object SignalToNoiseMode:
    def spectroscopy(sn: SignalToNoise, at: Wavelength): SignalToNoiseMode[ScienceMode.Spectroscopy.type] =
      new SignalToNoiseMode(sn, at)
    def imaging(sn: SignalToNoise): SignalToNoiseMode[ScienceMode.Imaging.type] =
      new SignalToNoiseMode(sn, ())

    def value[M <: ScienceMode]: Lens[SignalToNoiseMode[M], SignalToNoise] = Focus[SignalToNoiseMode[M]](_.value)
    def at[M <: ScienceMode]:    Lens[SignalToNoiseMode[M], AtForMode[M]]  = Focus[SignalToNoiseMode[M]](_.at)

    given [M <: ScienceMode](using Eq[AtForMode[M]]): Eq[SignalToNoiseMode[M]] =
      Eq.by(a => (a.value, a.at))

  object TimeAndCountMode:
    def spectroscopy(time: TimeSpan, count: PosInt, at: Wavelength): TimeAndCountMode[ScienceMode.Spectroscopy.type] =
      new TimeAndCountMode(time, count, at)
    def imaging(time: TimeSpan, count: PosInt): TimeAndCountMode[ScienceMode.Imaging.type] =
      new TimeAndCountMode(time, count, ())

    def time[M <: ScienceMode]:  Lens[TimeAndCountMode[M], TimeSpan]     = Focus[TimeAndCountMode[M]](_.time)
    def count[M <: ScienceMode]: Lens[TimeAndCountMode[M], PosInt]       = Focus[TimeAndCountMode[M]](_.count)
    def at[M <: ScienceMode]:    Lens[TimeAndCountMode[M], AtForMode[M]] = Focus[TimeAndCountMode[M]](_.at)

    given [M <: ScienceMode](using Eq[AtForMode[M]]): Eq[TimeAndCountMode[M]] =
      Eq.by(a => (a.time, a.count, a.at))

  given [M <: ScienceMode](using Eq[AtForMode[M]]): Eq[ExposureTimeMode[M]] = semiauto.eq

  def signalToNoise[M <: ScienceMode]: Prism[ExposureTimeMode[M], SignalToNoiseMode[M]] =
    GenPrism[ExposureTimeMode[M], SignalToNoiseMode[M]]

  def timeAndCount[M <: ScienceMode]: Prism[ExposureTimeMode[M], TimeAndCountMode[M]] =
    GenPrism[ExposureTimeMode[M], TimeAndCountMode[M]]

  def at[M <: ScienceMode]: Lens[ExposureTimeMode[M], AtForMode[M]] =
    Lens[ExposureTimeMode[M], AtForMode[M]](_.at)(newValue =>
        case sn: SignalToNoiseMode[M]   => SignalToNoiseMode.at[M].replace(newValue)(sn)
        case tc: TimeAndCountMode[M]    => TimeAndCountMode.at[M].replace(newValue)(tc)
    )

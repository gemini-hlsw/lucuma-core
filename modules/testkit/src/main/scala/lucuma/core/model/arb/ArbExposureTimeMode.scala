// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.all.PosInt
import lucuma.core.enums.ScienceMode
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.arb.ArbSignalToNoise
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbTimeSpan
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

import scala.reflect.TypeTest

trait ArbExposureTimeMode:
  import ExposureTimeMode.*
  import ArbRefined.given
  import ArbSignalToNoise.given
  import ArbTimeSpan.given
  import ArbWavelength.given

  def arbitrarySignalToNoiseModeSpectroscopy: Arbitrary[SignalToNoiseMode[ScienceMode.Spectroscopy.type]] =
    Arbitrary:
      for
        v <- arbitrary[SignalToNoise]
        w <- arbitrary[Wavelength]
      yield SignalToNoiseMode.spectroscopy(v, w)

  def arbitrarySignalToNoiseModeImaging: Arbitrary[SignalToNoiseMode[ScienceMode.Imaging.type]] =
    Arbitrary:
      for
        v <- arbitrary[SignalToNoise]
      yield SignalToNoiseMode.imaging(v)

  given arbsnmi: Arbitrary[SignalToNoiseMode[ScienceMode.Spectroscopy.type]] = arbitrarySignalToNoiseModeSpectroscopy
  given arbsnms: Arbitrary[SignalToNoiseMode[ScienceMode.Imaging.type]] = arbitrarySignalToNoiseModeImaging

  given TypeTest[Any, Wavelength] =
    new TypeTest[Any, Wavelength]:
      def unapply(s: Any): Option[s.type & Wavelength] = s match
        case q: (s.type & Wavelength) @unchecked => Some(q)
        case _ => None

  given [M <: ScienceMode]: Cogen[AtForMode[M]] =
    Cogen[Option[Wavelength]].contramap: a =>
      a match
        case w @ Wavelength(_) => Some(w)
        case _ => None

  given [M <: ScienceMode]: Cogen[SignalToNoiseMode[M]] =
    Cogen[(SignalToNoise, AtForMode[M])].contramap: a =>
      (
        a.value,
        a.at
      )

  def arbitraryTimeAndCountModeSpectroscopy: Arbitrary[TimeAndCountMode[ScienceMode.Spectroscopy.type]] =
    Arbitrary:
      for
        t <- arbitrary[TimeSpan]
        c <- arbitrary[PosInt]
        w <- arbitrary[Wavelength]
      yield TimeAndCountMode.spectroscopy(t, c, w)

  def arbitraryTimeAndCountModeImaging: Arbitrary[TimeAndCountMode[ScienceMode.Imaging.type]] =
    Arbitrary:
      for
        t <- arbitrary[TimeSpan]
        c <- arbitrary[PosInt]
      yield TimeAndCountMode.imaging(t, c)

  given arbtcms: Arbitrary[TimeAndCountMode[ScienceMode.Spectroscopy.type]] = arbitraryTimeAndCountModeSpectroscopy
  given arbtcmi: Arbitrary[TimeAndCountMode[ScienceMode.Imaging.type]] = arbitraryTimeAndCountModeImaging

  given [M <: ScienceMode]: Cogen[TimeAndCountMode[M]] =
    Cogen[(TimeSpan, PosInt, AtForMode[M])].contramap: a =>
      (
        a.time,
        a.count,
        a.at
      )

  def arbitraryExposureTimeModeSpectroscopy: Arbitrary[ExposureTimeMode[ScienceMode.Spectroscopy.type]] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[SignalToNoiseMode[ScienceMode.Spectroscopy.type]],
        arbitrary[TimeAndCountMode[ScienceMode.Spectroscopy.type]]
      )

  def arbitraryExposureTimeModeImaging: Arbitrary[ExposureTimeMode[ScienceMode.Imaging.type]] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[SignalToNoiseMode[ScienceMode.Imaging.type]],
        arbitrary[TimeAndCountMode[ScienceMode.Imaging.type]]
      )

  given arbetms: Arbitrary[ExposureTimeMode[ScienceMode.Spectroscopy.type]] = arbitraryExposureTimeModeSpectroscopy
  given arbetmi: Arbitrary[ExposureTimeMode[ScienceMode.Imaging.type]] = arbitraryExposureTimeModeImaging

  given [M <: ScienceMode]: Cogen[ExposureTimeMode[M]] =
    Cogen[(Option[SignalToNoiseMode[M]], Option[TimeAndCountMode[M]])].contramap: a =>
      (
        ExposureTimeMode.signalToNoise[M].getOption(a),
        ExposureTimeMode.timeAndCount[M].getOption(a)
      )

object ArbExposureTimeMode extends ArbExposureTimeMode

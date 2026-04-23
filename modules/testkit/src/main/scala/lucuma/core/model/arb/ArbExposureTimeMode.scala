// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import eu.timepit.refined.scalacheck.all.*
import eu.timepit.refined.types.all.PosInt
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.math.arb.ArbRefined
import lucuma.core.math.arb.ArbSignalToNoise
import lucuma.core.math.arb.ArbWavelength
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbTimeSpan
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbExposureTimeMode:
  import ExposureTimeMode.*
  import ArbRefined.given
  import ArbSignalToNoise.given
  import ArbTimeSpan.given
  import ArbWavelength.given

  given Arbitrary[SignalToNoiseMode] =
    Arbitrary:
      for
        v <- arbitrary[SignalToNoise]
        w <- arbitrary[Wavelength]
      yield SignalToNoiseMode(v, w)

  given Cogen[SignalToNoiseMode] =
    Cogen[(SignalToNoise, Wavelength)].contramap: a =>
      (
        a.value,
        a.at
      )

  given Arbitrary[TimeAndCountMode] =
    Arbitrary:
      for
        t <- arbitrary[TimeSpan]
        c <- arbitrary[PosInt]
        a <- arbitrary[PosInt]
        w <- arbitrary[Wavelength]
      yield TimeAndCountMode(t, c, a, w)

  given Cogen[TimeAndCountMode] =
    Cogen[(TimeSpan, PosInt, PosInt, Wavelength)].contramap: a =>
      (
        a.time,
        a.count,
        a.coadds,
        a.at
      )

  given Arbitrary[ExposureTimeMode] =
    Arbitrary:
      Gen.oneOf(arbitrary[SignalToNoiseMode], arbitrary[TimeAndCountMode])

  given Cogen[ExposureTimeMode] =
    Cogen[(Option[SignalToNoiseMode], Option[TimeAndCountMode])].contramap: a =>
      (
        ExposureTimeMode.signalToNoise.getOption(a),
        ExposureTimeMode.timeAndCount.getOption(a)
      )

object ArbExposureTimeMode extends ArbExposureTimeMode
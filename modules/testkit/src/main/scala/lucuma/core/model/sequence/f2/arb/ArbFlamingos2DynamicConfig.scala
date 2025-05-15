// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.flamingos2.arb

import lucuma.core.enums.*
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.flamingos2.arb.ArbFlamingos2FpuMask.given
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbTimeSpan.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbFlamingos2DynamicConfig:

  given Arbitrary[Flamingos2DynamicConfig] = Arbitrary(
    for
      exposure    <- arbitrary[TimeSpan]
      disperser   <- arbitrary[Option[Flamingos2Disperser]]
      filter      <- arbitrary[Flamingos2Filter]
      readMode    <- arbitrary[Flamingos2ReadMode]
      lyotWheel   <- arbitrary[Flamingos2LyotWheel]
      fpu         <- arbitrary[Flamingos2FpuMask]
      decker      <- arbitrary[Flamingos2Decker]
      readoutMode <- arbitrary[Flamingos2ReadoutMode]
      reads       <- arbitrary[Flamingos2Reads]
    yield Flamingos2DynamicConfig(exposure, disperser, filter, readMode, lyotWheel, fpu, decker, readoutMode, reads)
  )

  given Cogen[Flamingos2DynamicConfig] =
    Cogen[(TimeSpan, Option[Flamingos2Disperser], Flamingos2Filter, Flamingos2ReadMode, Flamingos2LyotWheel, Flamingos2FpuMask, Flamingos2Decker, Flamingos2ReadoutMode, Flamingos2Reads)]
      .contramap(s => (s.exposure, s.disperser, s.filter, s.readMode, s.lyotWheel, s.fpu, s.decker, s.readoutMode, s.reads))

object ArbFlamingos2DynamicConfig extends ArbFlamingos2DynamicConfig

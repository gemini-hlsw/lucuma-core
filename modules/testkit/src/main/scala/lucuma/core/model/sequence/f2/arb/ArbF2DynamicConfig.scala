// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.f2.arb

import lucuma.core.enums.*
import lucuma.core.model.sequence.f2.F2DynamicConfig
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.core.util.arb.ArbTimeSpan.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen

trait ArbF2DynamicConfig:

  given Arbitrary[F2DynamicConfig] = Arbitrary(
    for
      exposure    <- arbitrary[TimeSpan]
      disperser   <- arbitrary[Option[F2Disperser]]
      filter      <- arbitrary[F2Filter]
      readMode    <- arbitrary[F2ReadMode]
      lyot        <- arbitrary[F2LyotWheel]
      fpu         <- arbitrary[Option[F2Fpu]]
      readoutMode <- arbitrary[Option[F2ReadoutMode]]
      reads       <- arbitrary[Option[F2Reads]]
    yield F2DynamicConfig(exposure, disperser, filter, readMode, lyot, fpu, readoutMode, reads)
  )

  given Cogen[F2DynamicConfig] =
    Cogen[(TimeSpan, Option[F2Disperser], F2Filter, F2ReadMode, F2LyotWheel, Option[F2Fpu], Option[F2ReadoutMode], Option[F2Reads])]
      .contramap(s => (s.exposure, s.disperser, s.filter, s.readMode, s.lyot, s.fpu, s.readoutMode, s.reads))

object ArbF2DynamicConfig extends ArbF2DynamicConfig

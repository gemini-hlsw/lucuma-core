// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package ghost
package arb

import eu.timepit.refined.scalacheck.numeric.given
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostReadMode
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbTimeSpan
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbGhostDetector:

  import ArbEnumerated.given
  import ArbTimeSpan.given

  given Arbitrary[GhostDetector] =
    Arbitrary:
      for
        t <- arbitrary[TimeSpan]
        c <- arbitrary[PosInt]
        b <- arbitrary[GhostBinning]
        r <- arbitrary[GhostReadMode]
      yield GhostDetector(t, c, b, r)

  given Cogen[GhostDetector] =
    Cogen[(
      TimeSpan,
      Int,
      GhostBinning,
      GhostReadMode
    )].contramap: a =>
      (
        a.exposureTime,
        a.exposureCount.value,
        a.binning,
        a.readMode
      )

object ArbGhostDetector extends ArbGhostDetector
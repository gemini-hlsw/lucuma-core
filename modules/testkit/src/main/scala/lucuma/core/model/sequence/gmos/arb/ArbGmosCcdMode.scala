// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos
package arb

import lucuma.core.enums.*
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.*
import org.scalacheck.Arbitrary.*

trait ArbGmosCcdMode {
  import ArbEnumerated.given

  given Arbitrary[GmosCcdMode] =
    Arbitrary(
      for {
        xBin        <- arbitrary[GmosXBinning]
        yBin        <- arbitrary[GmosYBinning]
        ampCount    <- arbitrary[GmosAmpCount]
        ampGain     <- arbitrary[GmosAmpGain]
        ampReadMode <- arbitrary[GmosAmpReadMode]
      } yield GmosCcdMode(xBin, yBin, ampCount, ampGain, ampReadMode)
    )

  given Cogen[GmosCcdMode] =
    Cogen[(GmosXBinning, GmosYBinning, GmosAmpCount, GmosAmpGain, GmosAmpReadMode)].contramap(c =>
      (c.xBin, c.yBin, c.ampCount, c.ampGain, c.ampReadMode)
    )
}

object ArbGmosCcdMode extends ArbGmosCcdMode

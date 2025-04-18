// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence
package gmos
package arb

import cats.syntax.all.*
import lucuma.core.enums.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbTimeSpan
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbDynamicConfig {
  import ArbGmosCcdMode.given
  import ArbEnumerated.given
  import ArbTimeSpan.given
  import ArbGmosGratingConfig.given
  import ArbGmosFpuMask.given

  given Arbitrary[DynamicConfig.GmosNorth] = Arbitrary(
    for {
      exposure      <- arbitrary[TimeSpan]
      readout       <- arbitrary[GmosCcdMode]
      dtax          <- arbitrary[GmosDtax]
      roi           <- arbitrary[GmosRoi]
      gratingConfig <- arbitrary[Option[GmosGratingConfig.North]]
      filter        <- arbitrary[Option[GmosNorthFilter]]
      fpu           <- arbitrary[Option[GmosFpuMask[GmosNorthFpu]]]
    } yield DynamicConfig.GmosNorth(exposure, readout, dtax, roi, gratingConfig, filter, fpu)
  )

  given Cogen[DynamicConfig.GmosNorth] =
    Cogen[
      (TimeSpan,
       GmosCcdMode,
       GmosDtax,
       GmosRoi,
       Option[GmosGratingConfig.North],
       Option[GmosNorthFilter],
       Option[GmosFpuMask[GmosNorthFpu]]
      )
    ].contramap(c => (c.exposure, c.readout, c.dtax, c.roi, c.gratingConfig, c.filter, c.fpu))

  given Arbitrary[DynamicConfig.GmosSouth] = Arbitrary(
    for {
      exposure      <- arbitrary[TimeSpan]
      readout       <- arbitrary[GmosCcdMode]
      dtax          <- arbitrary[GmosDtax]
      roi           <- arbitrary[GmosRoi]
      gratingConfig <- arbitrary[Option[GmosGratingConfig.South]]
      filter        <- arbitrary[Option[GmosSouthFilter]]
      fpu           <- arbitrary[Option[GmosFpuMask[GmosSouthFpu]]]
    } yield DynamicConfig.GmosSouth(exposure, readout, dtax, roi, gratingConfig, filter, fpu)
  )

  given Cogen[DynamicConfig.GmosSouth] = Cogen[
    (TimeSpan,
     GmosCcdMode,
     GmosDtax,
     GmosRoi,
     Option[GmosGratingConfig.South],
     Option[GmosSouthFilter],
     Option[GmosFpuMask[GmosSouthFpu]]
    )
  ].contramap(c => (c.exposure, c.readout, c.dtax, c.roi, c.gratingConfig, c.filter, c.fpu))

  given Arbitrary[DynamicConfig] = Arbitrary(
    Gen.oneOf(
      arbitrary[DynamicConfig.GmosNorth],
      arbitrary[DynamicConfig.GmosSouth]
    )
  )

  given Cogen[DynamicConfig] =
    Cogen[Either[DynamicConfig.GmosNorth, DynamicConfig.GmosSouth]].contramap {
      case c @ DynamicConfig.GmosNorth(_, _, _, _, _, _, _) => c.asLeft
      case c @ DynamicConfig.GmosSouth(_, _, _, _, _, _, _) => c.asRight
    }
}

object ArbDynamicConfig extends ArbDynamicConfig

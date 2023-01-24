// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import cats.syntax.all._
import lucuma.core.arb.ArbTime
import lucuma.core.enums._
import lucuma.core.model.sequence.DynamicConfig
import lucuma.core.model.sequence._
import lucuma.core.util.TimeSpan
import lucuma.core.util.arb.ArbEnumerated
import lucuma.core.util.arb.ArbTimeSpan
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbDynamicConfig {
  import ArbGmosCcdMode._
  import ArbEnumerated._
  import ArbTimeSpan.given
  import ArbGmosGratingConfig._
  import ArbGmosFpuMask._
  import ArbTime._

  implicit val arbDynamicConfigGmosNorth: Arbitrary[DynamicConfig.GmosNorth] = Arbitrary(
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

  implicit val cogDynamicConfigGmosNorth: Cogen[DynamicConfig.GmosNorth] =
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

  implicit val arbDynamicConfigGmosSouth: Arbitrary[DynamicConfig.GmosSouth] = Arbitrary(
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

  implicit val cogDynamicConfigGmosSouth: Cogen[DynamicConfig.GmosSouth] = Cogen[
    (TimeSpan,
     GmosCcdMode,
     GmosDtax,
     GmosRoi,
     Option[GmosGratingConfig.South],
     Option[GmosSouthFilter],
     Option[GmosFpuMask[GmosSouthFpu]]
    )
  ].contramap(c => (c.exposure, c.readout, c.dtax, c.roi, c.gratingConfig, c.filter, c.fpu))

  implicit val arbDynamicConfig: Arbitrary[DynamicConfig] = Arbitrary(
    Gen.oneOf(
      arbitrary[DynamicConfig.GmosNorth],
      arbitrary[DynamicConfig.GmosSouth]
    )
  )

  implicit val cogDynamicConfig: Cogen[DynamicConfig] =
    Cogen[Either[DynamicConfig.GmosNorth, DynamicConfig.GmosSouth]].contramap {
      case c @ DynamicConfig.GmosNorth(_, _, _, _, _, _, _) => c.asLeft
      case c @ DynamicConfig.GmosSouth(_, _, _, _, _, _, _) => c.asRight
    }
}

object ArbDynamicConfig extends ArbDynamicConfig

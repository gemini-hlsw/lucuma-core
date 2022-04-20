// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.arb

import cats.syntax.all._
import lucuma.core.arb.ArbTime
import lucuma.core.enum._
import lucuma.core.model.sequence.InstrumentConfig
import lucuma.core.model.sequence._
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

import java.time.Duration

trait ArbInstrumentConfig {
  import ArbGmosCcdMode._
  import ArbEnumerated._
  import ArbGmosGrating._
  import ArbGmosFpuMask._
  import ArbTime._

  implicit val arbInstrumentConfigGmosNorth: Arbitrary[InstrumentConfig.GmosNorth] = Arbitrary(
    for {
      exposure <- arbitrary[Duration]
      readout  <- arbitrary[GmosCcdMode]
      dtax     <- arbitrary[GmosDtax]
      roi      <- arbitrary[GmosRoi]
      grating  <- arbitrary[Option[GmosGrating.North]]
      filter   <- arbitrary[Option[GmosNorthFilter]]
      fpu      <- arbitrary[Option[GmosFpuMask[GmosNorthFpu]]]
    } yield InstrumentConfig.GmosNorth(exposure, readout, dtax, roi, grating, filter, fpu)
  )

  implicit val cogInstrumentConfigGmosNorth: Cogen[InstrumentConfig.GmosNorth] =
    Cogen[
      (Duration,
       GmosCcdMode,
       GmosDtax,
       GmosRoi,
       Option[GmosGrating.North],
       Option[GmosNorthFilter],
       Option[GmosFpuMask[GmosNorthFpu]]
      )
    ].contramap(c => (c.exposure, c.readout, c.dtax, c.roi, c.grating, c.filter, c.fpu))

  implicit val arbInstrumentConfigGmosSouth: Arbitrary[InstrumentConfig.GmosSouth] = Arbitrary(
    for {
      exposure <- arbitrary[Duration]
      readout  <- arbitrary[GmosCcdMode]
      dtax     <- arbitrary[GmosDtax]
      roi      <- arbitrary[GmosRoi]
      grating  <- arbitrary[Option[GmosGrating.South]]
      filter   <- arbitrary[Option[GmosSouthFilter]]
      fpu      <- arbitrary[Option[GmosFpuMask[GmosSouthFpu]]]
    } yield InstrumentConfig.GmosSouth(exposure, readout, dtax, roi, grating, filter, fpu)
  )

  implicit val cogInstrumentConfigGmosSouth: Cogen[InstrumentConfig.GmosSouth] = Cogen[
    (Duration,
     GmosCcdMode,
     GmosDtax,
     GmosRoi,
     Option[GmosGrating.South],
     Option[GmosSouthFilter],
     Option[GmosFpuMask[GmosSouthFpu]]
    )
  ].contramap(c => (c.exposure, c.readout, c.dtax, c.roi, c.grating, c.filter, c.fpu))

  implicit val arbInstrumentConfig: Arbitrary[InstrumentConfig] = Arbitrary(
    Gen.oneOf(
      arbitrary[InstrumentConfig.GmosNorth],
      arbitrary[InstrumentConfig.GmosSouth]
    )
  )

  implicit val cogInstrumentConfig: Cogen[InstrumentConfig] =
    Cogen[Either[InstrumentConfig.GmosNorth, InstrumentConfig.GmosSouth]].contramap {
      case c @ InstrumentConfig.GmosNorth(_, _, _, _, _, _, _) => c.asLeft
      case c @ InstrumentConfig.GmosSouth(_, _, _, _, _, _, _) => c.asRight
    }
}

object ArbInstrumentConfig extends ArbInstrumentConfig

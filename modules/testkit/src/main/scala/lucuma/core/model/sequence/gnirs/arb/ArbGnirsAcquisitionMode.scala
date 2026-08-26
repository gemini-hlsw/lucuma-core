// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gnirs.arb

import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset.given
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMode
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbGnirsAcquisitionMode:
  given Arbitrary[GnirsAcquisitionMode.Faint] = Arbitrary:
    arbitrary[Offset].map(skyOffset => GnirsAcquisitionMode.Faint(skyOffset))

  given Cogen[GnirsAcquisitionMode.Faint] =
    Cogen[Offset].contramap(_.skyOffset)

  given Arbitrary[GnirsAcquisitionMode] = Arbitrary:
    Gen.oneOf(
      Gen.const(GnirsAcquisitionMode.VeryBright),
      Gen.const(GnirsAcquisitionMode.Bright),
      arbitrary[GnirsAcquisitionMode.Faint]
    )

  given Cogen[GnirsAcquisitionMode] =
    Cogen[Either[Unit, Either[Unit, GnirsAcquisitionMode.Faint]]]
      .contramap:
        case GnirsAcquisitionMode.VeryBright   => Left(())
        case GnirsAcquisitionMode.Bright       => Right(Left(()))
        case m @ GnirsAcquisitionMode.Faint(_) => Right(Right(m))

object ArbGnirsAcquisitionMode extends ArbGnirsAcquisitionMode
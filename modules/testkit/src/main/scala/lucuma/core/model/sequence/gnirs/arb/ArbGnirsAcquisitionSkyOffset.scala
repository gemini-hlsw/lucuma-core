// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gnirs.arb

import cats.syntax.option.*
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset.given
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionSkyOffset
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbGnirsAcquisitionSkyOffset:
  given Arbitrary[GnirsAcquisitionSkyOffset.Enabled] = Arbitrary:
    arbitrary[Offset].map(GnirsAcquisitionSkyOffset.Enabled(_))

  given Cogen[GnirsAcquisitionSkyOffset.Enabled] =
    Cogen[Offset].contramap(_.offset)

  given Arbitrary[GnirsAcquisitionSkyOffset] = Arbitrary:
    Gen.oneOf(
      Gen.const(GnirsAcquisitionSkyOffset.Disabled),
      arbitrary[GnirsAcquisitionSkyOffset.Enabled]
    )

  given Cogen[GnirsAcquisitionSkyOffset] =
    Cogen[Option[GnirsAcquisitionSkyOffset.Enabled]].contramap:
      case GnirsAcquisitionSkyOffset.Disabled => None
      case e: GnirsAcquisitionSkyOffset.Enabled => e.some

object ArbGnirsAcquisitionSkyOffset extends ArbGnirsAcquisitionSkyOffset
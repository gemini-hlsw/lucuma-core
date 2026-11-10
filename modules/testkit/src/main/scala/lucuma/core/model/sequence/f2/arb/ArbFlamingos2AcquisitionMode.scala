// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.flamingos2.arb

import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbOffset.given
import lucuma.core.model.sequence.flamingos2.Flamingos2AcquisitionMode
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbFlamingos2AcquisitionMode:
  given Arbitrary[Flamingos2AcquisitionMode.Faint] = Arbitrary:
    arbitrary[Offset].map(skyOffset => Flamingos2AcquisitionMode.Faint(skyOffset))

  given Cogen[Flamingos2AcquisitionMode.Faint] =
    Cogen[Offset].contramap(_.skyOffset)

  given Arbitrary[Flamingos2AcquisitionMode] = Arbitrary:
    Gen.oneOf(
      Gen.const(Flamingos2AcquisitionMode.Bright),
      arbitrary[Flamingos2AcquisitionMode.Faint]
    )

  given Cogen[Flamingos2AcquisitionMode] =
    Cogen[Option[Flamingos2AcquisitionMode.Faint]].contramap(Flamingos2AcquisitionMode.faint.getOption)

object ArbFlamingos2AcquisitionMode extends ArbFlamingos2AcquisitionMode

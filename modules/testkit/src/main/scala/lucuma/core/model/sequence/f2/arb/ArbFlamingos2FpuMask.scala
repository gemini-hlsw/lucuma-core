// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.flamingos2.arb

import cats.syntax.all.*
import eu.timepit.refined.scalacheck.string.given
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.*
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.util.Enumerated
import lucuma.core.util.arb.ArbEnumerated.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbFlamingos2FpuMask:

  given Arbitrary[Flamingos2FpuMask] =
    Arbitrary(
      Gen.oneOf[Flamingos2FpuMask](
        Flamingos2FpuMask.Imaging,
        Gen.oneOf(Enumerated[Flamingos2Fpu].all).map(Flamingos2FpuMask.Builtin(_)),
        for
          f <- arbitrary[NonEmptyString]
          s <- arbitrary[Flamingos2CustomSlitWidth]
        yield Flamingos2FpuMask.Custom(f, s)
      )
    )

  given Cogen[Flamingos2FpuMask] =
    Cogen[Option[Either[Flamingos2Fpu, (String, Flamingos2CustomSlitWidth)]]]
      .contramap {
        case Flamingos2FpuMask.Imaging => none
        case Flamingos2FpuMask.Builtin(b) => b.asLeft.some
        case Flamingos2FpuMask.Custom(f, s) => (f.value, s).asRight.some
      }

object ArbFlamingos2FpuMask extends ArbFlamingos2FpuMask

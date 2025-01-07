// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.f2.arb

import cats.syntax.all.*
import eu.timepit.refined.scalacheck.string.given
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.*
import lucuma.core.model.sequence.f2.F2FpuMask
import lucuma.core.util.Enumerated
import lucuma.core.util.arb.ArbEnumerated.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbF2FpuMask:

  given Arbitrary[F2FpuMask] =
    Arbitrary(
      Gen.oneOf[F2FpuMask](
        F2FpuMask.Imaging,
        Gen.oneOf(Enumerated[F2Fpu].all).map(F2FpuMask.Builtin(_)),
        for
          f <- arbitrary[Option[NonEmptyString]]
          s <- arbitrary[F2CustomSlitWidth]
        yield F2FpuMask.Custom(f, s)
      )
    )

  given Cogen[F2FpuMask] =
    Cogen[Option[Either[F2Fpu, (Option[String], F2CustomSlitWidth)]]]
      .contramap {
        case F2FpuMask.Imaging => none
        case F2FpuMask.Builtin(b) => b.asLeft.some
        case F2FpuMask.Custom(f, s) => (f.map(_.value), s).asRight.some
      }

object ArbF2FpuMask extends ArbF2FpuMask

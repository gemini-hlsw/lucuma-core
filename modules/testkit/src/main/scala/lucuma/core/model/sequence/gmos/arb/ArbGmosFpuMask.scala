// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos
package arb

import cats.syntax.all.*
import lucuma.core.enums.GmosCustomSlitWidth
import lucuma.core.model.MaskDefinition
import lucuma.core.model.arb.ArbMaskDefinition
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbGmosFpuMask {
  import ArbEnumerated.given
  import ArbMaskDefinition.given

  given arbGmosBuiltinFpuMask[T: Arbitrary]: Arbitrary[GmosFpuMask.Builtin[T]] =
    Arbitrary(arbitrary[T].map(GmosFpuMask.Builtin.apply))

  given Arbitrary[GmosFpuMask.Custom] =
    Arbitrary(
      for {
        mask      <- arbitrary[MaskDefinition]
        slitWidth <- arbitrary[GmosCustomSlitWidth]
      } yield GmosFpuMask.Custom(mask, slitWidth)
    )

  given arbGmosFpuMask[T: Arbitrary]: Arbitrary[GmosFpuMask[T]] =
    Arbitrary(
      Gen.oneOf(
        arbitrary[GmosFpuMask.Builtin[T]],
        arbitrary[GmosFpuMask.Custom]
      )
    )

  given cogGmosBuiltinFpuMask[T: Cogen]: Cogen[GmosFpuMask.Builtin[T]] =
    Cogen[T].contramap(_.value)

  given Cogen[GmosFpuMask.Custom] =
    Cogen[(MaskDefinition, GmosCustomSlitWidth)].contramap(m => (m.mask, m.slitWidth))

  given cogGmosFpuMask[T: Cogen]: Cogen[GmosFpuMask[T]] =
    Cogen[Either[GmosFpuMask.Builtin[T], GmosFpuMask.Custom]]
      .contramap {
        case m @ GmosFpuMask.Builtin(_)   => m.asLeft
        case m @ GmosFpuMask.Custom(_, _) => m.asRight
      }
}

object ArbGmosFpuMask extends ArbGmosFpuMask

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos
package arb

import cats.syntax.all.*
import eu.timepit.refined.scalacheck.string.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.GmosCustomSlitWidth
import lucuma.core.math.arb.ArbRefined
import lucuma.core.util.arb.ArbEnumerated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.*

trait ArbGmosFpuMask {
  import ArbEnumerated._
  import ArbRefined.given

  implicit def arbGmosBuiltinFpuMask[T: Arbitrary]: Arbitrary[GmosFpuMask.Builtin[T]] =
    Arbitrary(arbitrary[T].map(GmosFpuMask.Builtin.apply))

  implicit val arbGmosCustomFpuMask: Arbitrary[GmosFpuMask.Custom] =
    Arbitrary(
      for {
        filename  <- arbitrary[NonEmptyString]
        slitWidth <- arbitrary[GmosCustomSlitWidth]
      } yield GmosFpuMask.Custom(filename, slitWidth)
    )

  implicit def arbGmosFpuMask[T: Arbitrary]: Arbitrary[GmosFpuMask[T]] =
    Arbitrary(
      Gen.oneOf(
        arbitrary[GmosFpuMask.Builtin[T]],
        arbitrary[GmosFpuMask.Custom]
      )
    )

  implicit def cogGmosBuiltinFpuMask[T: Cogen]: Cogen[GmosFpuMask.Builtin[T]] =
    Cogen[T].contramap(_.value)

  implicit val cogGmosCustomFpuMask: Cogen[GmosFpuMask.Custom] =
    Cogen[(NonEmptyString, GmosCustomSlitWidth)].contramap(m => (m.filename, m.slitWidth))

  implicit def cogGmosFpuMask[T: Cogen]: Cogen[GmosFpuMask[T]] =
    Cogen[Either[GmosFpuMask.Builtin[T], GmosFpuMask.Custom]]
      .contramap {
        case m @ GmosFpuMask.Builtin(_)   => m.asLeft
        case m @ GmosFpuMask.Custom(_, _) => m.asRight
      }
}

object ArbGmosFpuMask extends ArbGmosFpuMask

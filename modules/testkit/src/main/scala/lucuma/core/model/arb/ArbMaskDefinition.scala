// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model
package arb

import cats.syntax.all.*
import lucuma.core.util.arb.ArbGid
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbMaskDefinition {
  import ArbGid.given

  given Arbitrary[MaskDefinition] =
    Arbitrary(
      Gen.oneOf(
        Gen.const(ToBeDefined),
        arbitrary[Attachment.Id].map(Defined(_))
      )
    )

  given Cogen[MaskDefinition] =
    Cogen[Option[Attachment.Id]].contramap {
      case ToBeDefined  => none
      case Defined(id)  => id.some
    }
}

object ArbMaskDefinition extends ArbMaskDefinition

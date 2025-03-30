// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util.arb

import lucuma.core.util.NewTypeGen
import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary

trait ArbNewType:
  given [A, W](using gen: NewTypeGen[A, W], arbw: Arbitrary[W]): Arbitrary[A] =
    Arbitrary(arbitrary[W].map(gen.wrap(_)))

  given [A, W](using gen: NewTypeGen[A, W], cogenw: Cogen[W]): Cogen[A] =
    Cogen[W].contramap(gen.unwrap(_))

object ArbNewType extends ArbNewType
  

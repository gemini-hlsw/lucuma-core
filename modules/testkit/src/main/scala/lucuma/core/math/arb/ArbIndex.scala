// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import lucuma.core.math.Index
import lucuma.core.optics.syntax.prism.*
import org.scalacheck.*
import org.scalacheck.Gen.*

trait ArbIndex {

  val genIndex: Gen[Index] =
    choose[Short](1, Short.MaxValue).map(Index.fromShort.unsafeGet)

  given Arbitrary[Index] =
    Arbitrary(genIndex)

  given Cogen[Index] =
    Cogen[Short].contramap(_.toShort)

}

object ArbIndex extends ArbIndex

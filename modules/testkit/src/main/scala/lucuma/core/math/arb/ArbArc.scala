// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import lucuma.core.math.Angular
import lucuma.core.math.Arc
import lucuma.core.math.Arc.Empty
import lucuma.core.math.Arc.Full
import lucuma.core.math.Arc.Partial
import org.scalacheck.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen.*

trait ArbArc:

  def genPartial[A: Arbitrary: Angular]: Gen[Arc.Partial[A]] =
    arbitrary[(A,A)].map(Arc.Partial(_, _))

  given [A: Arbitrary: Angular]: Arbitrary[Arc.Partial[A]] =
    Arbitrary(genPartial)
      
  given [A: Arbitrary: Angular]: Arbitrary[Arc[A]] =
    Arbitrary:
      Gen.frequency(
        1  -> Gen.const(Arc.Empty()),
        1  -> Gen.const(Arc.Full()),
        10 -> genPartial
      )
      
  given [A: Cogen: Angular]: Cogen[Arc[A]] =
    Cogen[Long].contramap:
      case Empty() => Long.MinValue
      case Full() => Long.MaxValue
      case Partial(start, end) => start.toAngle.toMicroarcseconds ^ end.toAngle.toMicroarcseconds
      
object ArbArc extends ArbArc

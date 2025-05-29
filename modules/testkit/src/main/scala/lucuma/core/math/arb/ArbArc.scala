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

  def genEmpty[A]: Gen[Arc.Empty[A]] =
    Gen.const(Arc.Empty())

  def genFull[A]: Gen[Arc.Full[A]] =
    Gen.const(Arc.Full())
  
  def genPartial[A: Arbitrary: Angular]: Gen[Arc.Partial[A]] =
    arbitrary[(A,A)].map(Arc.Partial(_, _))

  given [A]: Arbitrary[Arc.Empty[A]] =
    Arbitrary(genEmpty)

  given [A]: Arbitrary[Arc.Full[A]] =
    Arbitrary(genFull)

  given [A: Arbitrary: Angular]: Arbitrary[Arc.Partial[A]] =
    Arbitrary(genPartial)

  given [A]: Cogen[Empty[A]] =
    Cogen[Long].contramap(_ => 0L)
  
  given [A]: Cogen[Full[A]] =
    Cogen[Long].contramap(_ => Long.MaxValue)
  
  given [A: Cogen]: Cogen[Partial[A]] =
    Cogen[(A, A)].contramap(a => (a.start, a.end))

  given [A: Arbitrary: Angular]: Arbitrary[Arc[A]] =
    Arbitrary:
      Gen.frequency(
        1  -> genEmpty,
        1  -> genFull,
        10 -> genPartial
      )
      
  given [A: Cogen: Angular]: Cogen[Arc[A]] =
    Cogen: (seed, arc) => 
      arc match
        case e @ Empty() => Cogen[Empty[A]].perturb(seed, e)
        case f @ Full() => Cogen[Full[A]].perturb(seed, f)
        case p @ Partial(_, _) => Cogen[Arc.Partial[A]].perturb(seed, p)
      
object ArbArc extends ArbArc

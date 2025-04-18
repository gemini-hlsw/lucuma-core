// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.arb

import coulomb.Quantity
import eu.timepit.refined.api.Max
import eu.timepit.refined.api.Min
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.Validate
import eu.timepit.refined.internal.Adjacent
import eu.timepit.refined.predicates.boolean.Or
import lucuma.core.util.NewTypeGen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait ArbRefined {
  given arbRefinedOr[T, P1, P2](using Arbitrary[T Refined P1], Arbitrary[T Refined P2]): Arbitrary[T Refined (P1 Or P2)] =
    Arbitrary:
      Gen.oneOf(
        arbitrary[T Refined P1].map(_.asInstanceOf[T Refined (P1 Or P2)]),
        arbitrary[T Refined P2].map(_.asInstanceOf[T Refined (P1 Or P2)])
      )

  given Adjacent[BigDecimal] = Adjacent.instance[BigDecimal](
    _.compare(_),
    x => BigDecimal(Adjacent[Double].nextUp(x.toDouble)),
    x => BigDecimal(Adjacent[Double].nextDown(x.toDouble))
  )

  given Max[BigDecimal] = new Max[BigDecimal]:
    val max: BigDecimal = BigDecimal(Double.MaxValue)

  given Min[BigDecimal] = new Min[BigDecimal]:
    val min: BigDecimal = BigDecimal(Double.MinValue)

  // Arbitrary for doubly refined types. We assume P1 is more restrictive than P.
  given arbRefinedRefined[T, P, P1](using valp: Validate[T, P], arbp1: Arbitrary[T Refined P1]): Arbitrary[T Refined P Refined P1] = 
    Arbitrary(arbitrary[T Refined P1].suchThat(r => valp.isValid(r.value)).map(_.asInstanceOf[T Refined P Refined P1]))

  // Arbitrary for refined `Quantity`s based on refining the underlying type.
  // In other words, `Quantity[T Refined P, U]` is equivalent to `Quantity[T, U] Refined P`.
  given arbRefinedQuantity[T, U, P](using arbq: Arbitrary[Quantity[T Refined P, U]]): Arbitrary[Quantity[T, U] Refined P] =
    Arbitrary(arbitrary[Quantity[T Refined P, U]].map(_.asInstanceOf[Quantity[T, U] Refined P]))

  // Arbitrary for refined `NewType`s based on refining the underlying type.
  given arbRefinedNewType[A, W, P](using nt: NewTypeGen[A, W], arbw: Arbitrary[W Refined P]): Arbitrary[A Refined P] =
    Arbitrary(arbitrary[W Refined P].map(r => nt.wrap(r.value).asInstanceOf[A Refined P]))

  given cogenRefined[A: Cogen, P]: Cogen[A Refined P] =
    Cogen[A].contramap(_.value)
}

object ArbRefined extends ArbRefined

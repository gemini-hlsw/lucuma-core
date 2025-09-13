// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.refined

import coulomb.Quantity
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.RefinedType
import eu.timepit.refined.api.Validate
import lucuma.core.util.NewTypeGen
import cats.kernel.Order
import cats.kernel.Hash
import eu.timepit.refined.api.RefType
import eu.timepit.refined.cats.derivation.refTypeViaContravariant

type RefinedTypeAux[T, P] = RefinedType.Aux[Refined[T, P], Refined, T, P]

// A `Refined[T, P]` can be further refined with predicate `P1` if `T` can be refined with predicate `P1`.
given refinedValidate[T, P, FTP <: Refined[T, P], P1](using v: Validate[T, P1]): Validate[FTP, P1] =
  Validate.instance(e => v.validate(e.value), e => v.showExpr(e.value))

// A `Quantity[T, U]` can be refined with predicate `P` if `T` can be refined with predicate `P`.
given quantityValidate[T, P, U](using v: Validate[T, P]): Validate[Quantity[T, U], P] =
  Validate.instance(e => v.validate(e.value), e => v.showExpr(e.value))

// A `NewType[W]` can be refined with predicate `P` if `W` can be refined with predicate `P`.
given newTypeValidate[A, W, P](using n: NewTypeGen[A, W], v: Validate[W, P]): Validate[A, P] =
  Validate.instance(e => v.validate(n.unwrap(e)), e => v.showExpr(n.unwrap(e)))

given hashForRefined[T, P](using hashT: Hash[T], rt: RefType[Refined]): Hash[Refined[T, P]] =
  refTypeViaContravariant[Refined, Hash, T, P]

// New coulomb needs an instance of `Order[T] & Hash[T]` to get refined coulomb quantities
// to get the correct instances to play with cats
given [T, P](using
    orderT: Order[T],
    hashT: Hash[T],
    rt: RefType[Refined]
): (Order[Refined[T, P]] & Hash[Refined[T, P]]) =
    new Order[Refined[T, P]] with Hash[Refined[T, P]]:
        // Use refined-cats Order instance directly
        private val orderInstance = eu.timepit.refined.cats.refTypeOrder[Refined, T, P](using orderT, rt)
        private val hashInstance = hashForRefined[T, P](using hashT, rt)

        def compare(x: Refined[T, P], y: Refined[T, P]): Int =
            orderInstance.compare(x, y)
        def hash(x: Refined[T, P]): Int =
            hashInstance.hash(x)

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.Eq
import cats.Monoid
import cats.Order
import coulomb.Quantity
import eu.timepit.refined.api.*
import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.optics.quantityIso
import lucuma.core.optics.refinedPrism
import lucuma.core.refined.RefinedTypeAux
import monocle.Iso
import monocle.Prism

import scala.annotation.nowarn
import scala.util.NotGiven

/**
  * Typeclass to convert between a new type and the wrapped value.
  * Mostly used for deriving typeclasses not contemplated by `NewType`. For example: `Arbitrary` and `Cogen`.
  */
trait NewTypeGen[A, W]:
  def wrap(wrapped: W): A
  def unwrap(newType: A): W

/**
 * Usage:
 * ```
 * object Name extends NewType[String]
 * type Name = Name.Type
 * ```
 */
trait NewType[Wrapped]{
  opaque type Type = Wrapped

  inline def apply(w: Wrapped): Type = w

  val Value: Iso[Type, Wrapped] = Iso[Type, Wrapped](_.value)(apply)

  given NewTypeGen[Type, Wrapped] = new NewTypeGen[Type, Wrapped]:
    def wrap(w: Wrapped): Type = w
    def unwrap(t: Type): Wrapped = t


  extension (t: Type)
    inline def value: Wrapped                           = t
    inline def modifyValue(f: Wrapped => Wrapped): Type = apply(f(value))

  given (using CanEqual[Wrapped, Wrapped]): CanEqual[Type, Type]               = CanEqual.derived
  // Only provide an Eq instance if it doesn't also provide Order
  @nowarn
  given (using eq:   Eq[Wrapped], noOrder: NotGiven[Order[Wrapped]]): Eq[Type] = eq
  given (using enc:  Encoder[Wrapped]): Encoder[Type]                          = enc
  given (using dec:  Decoder[Wrapped]): Decoder[Type]                          = dec
  given (using disp: Display[Wrapped]): Display[Type]                          = disp
  given (using m:    Monoid[Wrapped]): Monoid[Type]                            = m
  given (using ord:  Order[Wrapped]): Order[Type]                              = ord
  given (using ord:  Ordering[Wrapped]): Ordering[Type]                        = ord
}

/**
 * Usage:
 * ```
 * object Probability extends NewRefined[BigDecimal, Interval.Closed[0, 1]]
 * type Probability = Probability.Type
 * ```
 */
trait NewRefined[T, P](using rt: RefinedTypeAux[T, P]) extends NewType[T Refined P]:
  type BaseType  = T
  type Predicate = P
  val From: Prism[T, Type]                   = refinedPrism(using rt.validate).andThen(Value.reverse)
  def from(t:       T): Either[String, Type] = rt.refine(t).map(apply(_))
  def unsafeFrom(x: T): Type                 = apply(rt.unsafeRefine(x))

/**
  * Usage:
  * ```
  * object Score extends NewRefinedQuantity[Int, Interval.Closed[0, 5], Stars]
  * type Score = Score.Type
  * ```
  */
trait NewRefinedQuantity[T, P, U](using rt: RefinedTypeAux[T, P]) extends NewType[Quantity[T Refined P, U]]:
  type BaseType  = T
  type Predicate = P
  type Units     = U
  val FromRefined: Iso[Refined[T, P], Type]                 = quantityIso.reverse.andThen(Value.reverse)
  val From: Prism[T, Type]                                  = refinedPrism(using rt.validate).andThen(FromRefined)
  val FromQuantity: Prism[Quantity[T, U], Type]             = quantityIso.andThen(From)
  def from(t:       T): Either[String, Type]                = rt.refine(t).map(quantityIso.reverseGet(_)).map(apply(_))
  def unsafeFrom(x: T): Type                                = apply(quantityIso.reverseGet(rt.unsafeRefine(x)))
  def fromQuantity(q: Quantity[T, U]): Either[String, Type] = from(q.value)
  def unsafeFromQuantity(q: Quantity[T, U]): Type           = unsafeFrom(q.value)
  def fromRefined(r: Refined[T, P]): Type                   = apply(quantityIso.reverseGet(r))

/**
 * Usage:
 * ```
 * object IsActive extends NewBoolean
 * type IsActive = IsActive.Type
 * ```
 * or, if you want more descriptive names than `True` and `False`:
 * ```
 * object ActiveStatus extends NewBoolean { inline def Active = True; inline def Inactive = False }
 * type ActiveStatus = ActiveStatus.Type
 * ```
 * or, if you want to be able to pattern-match (but allocate 2 vals):
 * ```
 * object ActiveStatus extends NewBoolean { val Active = True; val Inactive = False }
 * type ActiveStatus = ActiveStatus.Type
 * ```
 */
trait NewBoolean extends NewType[Boolean]:
  inline def True:  Type = apply(true)
  inline def False: Type = apply(false)

  extension (t: Type)
    def fold[A](whenTrue: => A, whenFalse: => A): A =
      if t then whenTrue else whenFalse

    def flip: Type = fold(False, True)

  given Conversion[Type, Boolean] = _.asInstanceOf[Boolean] // Runs into loop if not type-coerced

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import cats.Eq
import cats.Monoid
import cats.Order
import eu.timepit.refined.api.*
import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.optics.refinedPrism
import monocle.Iso
import monocle.Prism

import scala.util.NotGiven

// trait NewTypeGen[W, A <: NewType[W]]:
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

  val value: Iso[Type, Wrapped] = Iso[Type, Wrapped](_.value)(apply)

  given NewTypeGen[Type, Wrapped] = new NewTypeGen[Type, Wrapped]:
    def wrap(w: Wrapped): Type = w
    def unwrap(t: Type): Wrapped = t


  extension (t: Type)
    inline def value: Wrapped                           = t
    inline def modifyValue(f: Wrapped => Wrapped): Type = apply(f(value))

  given (using CanEqual[Wrapped, Wrapped]): CanEqual[Type, Type]               = CanEqual.derived
  // Only provide an Eq instance if it doesn't also provide Order
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
 * object Score extends RefinedNewType[Int, Not[Less[0]] And Not[Greater[5]]]
 * type Score = Score.Type
 * ```
 */
trait RefinedNewType[T, P](using RefinedType.AuxT[T Refined P, T]) extends NewType[T Refined P]:
  private val TypeOps                                  = new RefinedTypeOps[T Refined P, T]
  def from(using Validate[T, P]): Prism[T, Type]       = refinedPrism.andThen(value.reverse)
  def from(t:       T): Either[String, Type]           = TypeOps.from(t).map(apply(_))
  def unsafeFrom(x: T): Type                           = apply(TypeOps.unsafeFrom(x))

/**
 * Usage:
 * ```
 * object IsActive extends NewBool
 * type IsActive = IsActive.Type
 * ```
 * or, if you want more descriptive names than `True` and `False`:
 * ```
 * object ActiveStatus extends NewBool { inline def Active = True; inline def Inactive = False }
 * type ActiveStatus = ActiveStatus.Type
 * ```
 * or, if you want to be able to pattern-match (but allocate 2 vals):
 * ```
 * object ActiveStatus extends NewBool { val Active = True; val Inactive = False }
 * type ActiveStatus = ActiveStatus.Type
 * ```
 */
trait NewBool extends NewType[Boolean]:
  inline def True:  Type = apply(true)
  inline def False: Type = apply(false)

  given Conversion[Type, Boolean] = _.asInstanceOf[Boolean] // Runs into loop if not type-coerced

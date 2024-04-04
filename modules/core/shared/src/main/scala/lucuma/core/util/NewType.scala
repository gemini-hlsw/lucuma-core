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

/**
  * Usage:
  * ```
  * object Name extends NewType[String]
  * type Name = Name.Type
  * ```
  */
trait NewType[Wrapped]:
  opaque type Type = Wrapped

  inline def apply(w: Wrapped): Type = w

  val value: Iso[Type, Wrapped] = Iso[Type, Wrapped](_.value)(apply)

  extension (t: Type)
    inline def value: Wrapped = t
    inline def modifyValue(f: Wrapped => Wrapped): Type = apply(f(value))

  given (using CanEqual[Wrapped, Wrapped]): CanEqual[Type, Type] = CanEqual.derived
  given (using eq: Eq[Wrapped]): Eq[Type]                        = eq
  given (using enc: Encoder[Wrapped]): Encoder[Type]             = enc
  given (using dec: Decoder[Wrapped]): Decoder[Type]             = dec
  given (using disp: Display[Wrapped]): Display[Type]            = disp
  given (using m: Monoid[Wrapped]): Monoid[Type]                 = m
  given (using ord: Order[Wrapped]): Order[Type]                 = ord
  given (using ord: Ordering[Wrapped]): Ordering[Type]           = ord

trait RefinedNewType[T, P](using RefinedType.AuxT[T Refined P, T]) extends NewType[T Refined P]:
  private val TypeOps = new RefinedTypeOps[T Refined P, T]
  def from(using Validate[T, P]): Prism[T, Type] = refinedPrism.andThen(value.reverse)
  def from(t: T): Either[String, Type] = TypeOps.from(t).map(apply(_))
  def unsafeFrom(x: T): Type = apply(TypeOps.unsafeFrom(x))

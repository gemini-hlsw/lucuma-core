// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import io.circe.Encoder
import io.circe.Decoder
import lucuma.core.util.Display
import cats.Eq
import cats.Monoid
import monocle.Iso

/**
  * Usage:
  * ```
  * object Name extends NewType[String]
  * type Name = Name.Type
  * ```
  */
trait NewType[Wrapped]:
  opaque type Type = Wrapped

  def apply(w: Wrapped): Type = w

  extension (t: Type) inline def value: Wrapped = t

  val value: Iso[Type, Wrapped] = Iso[Type, Wrapped](_.value)(apply)

  given (using CanEqual[Wrapped, Wrapped]): CanEqual[Type, Type] = CanEqual.derived
  given (using eq: Eq[Wrapped]): Eq[Type]                        = eq
  given (using enc: Encoder[Wrapped]): Encoder[Type]             = enc
  given (using dec: Decoder[Wrapped]): Decoder[Type]             = dec
  given (using disp: Display[Wrapped]): Display[Type]            = disp
  given (using m: Monoid[Wrapped]): Monoid[Type]                 = m

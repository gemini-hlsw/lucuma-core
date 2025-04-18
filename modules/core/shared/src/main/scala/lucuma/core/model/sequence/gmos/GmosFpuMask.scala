// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gmos

import cats.Eq
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.GmosCustomSlitWidth
import monocle.Focus
import monocle.Iso
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

sealed trait GmosFpuMask[+T] {

  import GmosFpuMask.Builtin
  import GmosFpuMask.Custom

  def fold[A](
    g: Builtin[T] => A,
    f: Custom     => A
  ): A =
    this match {
      case b @ Builtin(_)   => g(b)
      case c @ Custom(_, _) => f(c)
    }

  def builtin: Option[Builtin[T]] =
    fold(_.some, _ => none)

  def builtinFpu: Option[T] =
    builtin.map(_.value)

  def custom: Option[Custom] =
    fold(_ => none, _.some)

  def customFilename: Option[NonEmptyString] =
    custom.map(_.filename)

  def customSlitWidth: Option[GmosCustomSlitWidth] =
    custom.map(_.slitWidth)

}

object GmosFpuMask {

  final case class Builtin[+T](value: T) extends GmosFpuMask[T]

  object Builtin {
    given [T: Eq]: Eq[Builtin[T]] =
      Eq.by(_.value)

    /** @group Optics */
    def value[T]: Iso[Builtin[T], T] =
      Iso[Builtin[T], T](_.value)(Builtin.apply)
  }

  final case class Custom(filename: NonEmptyString, slitWidth: GmosCustomSlitWidth) extends GmosFpuMask[Nothing]

  object Custom {
    given Eq[Custom] = Eq.by(x => (x.filename, x.slitWidth))

    /** @group Optics */
    val filename: Lens[Custom, NonEmptyString] =
      Focus[Custom](_.filename)

    /** @group Optics */
    val slitWidth: Lens[Custom, GmosCustomSlitWidth] =
      Focus[Custom](_.slitWidth)
  }

  given [T: Eq]: Eq[GmosFpuMask[T]] = Eq.instance {
    case (a @ Builtin(_), b @ Builtin(_))     => a === b
    case (a @ Custom(_, _), b @ Custom(_, _)) => a === b
    case _                                    => false
  }

  /** @group Optics */
  def builtin[T]: Prism[GmosFpuMask[T], T] =
    GenPrism[GmosFpuMask[T], GmosFpuMask.Builtin[T]].andThen(Builtin.value)

  /** @group Optics */
  def custom[T]: Prism[GmosFpuMask[T], Custom] =
    GenPrism[GmosFpuMask[T], Custom]
}

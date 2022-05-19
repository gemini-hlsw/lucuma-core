// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.syntax.all._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.`enum`.GmosCustomSlitWidth
import monocle.Focus
import monocle.Iso
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

sealed trait GmosFpuMask[+T]
object GmosFpuMask {
  final case class Builtin[+T](value: T) extends GmosFpuMask[T]
  object Builtin {
    implicit def eqGmosFpuMaskBuiltin[T: Eq]: Eq[Builtin[T]] =
      Eq.by(_.value)

    /** @group Optics */
    def value[T]: Iso[Builtin[T], T] =
      Iso[Builtin[T], T](_.value)(Builtin.apply)
  }

  final case class Custom(filename: NonEmptyString, slitWidth: GmosCustomSlitWidth)
      extends GmosFpuMask[Nothing]
  object Custom {
    implicit val eqGmosFpuMaskCustom: Eq[Custom] = Eq.by(x => (x.filename, x.slitWidth))

    /** @group Optics */
    val filename: Lens[Custom, NonEmptyString] =
      Focus[Custom](_.filename)

    /** @group Optics */
    val slitWidth: Lens[Custom, GmosCustomSlitWidth] =
      Focus[Custom](_.slitWidth)
  }

  implicit def eqGmosFpuMask[T: Eq]: Eq[GmosFpuMask[T]] = Eq.instance {
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

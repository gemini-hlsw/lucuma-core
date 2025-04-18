// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.f2

import cats.Eq
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.F2CustomSlitWidth
import lucuma.core.enums.F2Fpu
import monocle.Focus
import monocle.Iso
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

sealed trait F2FpuMask:

  import F2FpuMask.Imaging
  import F2FpuMask.Builtin
  import F2FpuMask.Custom

  inline def fold[A](
    i: => A,
    g: Builtin => A,
    f: Custom  => A
  ): A =
    this match
      case Imaging          => i
      case b @ Builtin(_)   => g(b)
      case c @ Custom(_, _) => f(c)

  def builtin: Option[Builtin] =
    fold(none, _.some, _ => none)

  def builtinFpu: Option[F2Fpu] =
    builtin.map(_.value)

  def custom: Option[Custom] =
    fold(none, _ => none, _.some)

  def customFilename: Option[NonEmptyString] =
    custom.flatMap(_.filename)

  def customSlitWidth: Option[F2CustomSlitWidth] =
    custom.map(_.slitWidth)

  def isImaging: Boolean =
    fold(true, _ => false, _ => false)

  def isMOS: Boolean =
    fold(false, _ => false, _ => true)

  def isLongSlit: Boolean =
    fold(false, _ => true, _ => false)

object F2FpuMask:
  case object Imaging extends F2FpuMask

  case class Builtin(value: F2Fpu) extends F2FpuMask

  object Builtin:
    given Eq[Builtin] =
      Eq.by(_.value)

    /** @group Optics */
    def value: Iso[Builtin, F2Fpu] =
      Iso[Builtin, F2Fpu](_.value)(Builtin.apply)

  case class Custom(filename: Option[NonEmptyString], slitWidth: F2CustomSlitWidth) extends F2FpuMask

  object Custom:
    given Eq[Custom] = Eq.by(x => (x.filename, x.slitWidth))

    /** @group Optics */
    val filename: Lens[Custom, Option[NonEmptyString]] =
      Focus[Custom](_.filename)

    /** @group Optics */
    val customSlitWidth: Lens[Custom, F2CustomSlitWidth] =
      Focus[Custom](_.slitWidth)

  given Eq[F2FpuMask] = Eq.instance:
    case (Imaging, Imaging)                   => true
    case (a @ Builtin(_), b @ Builtin(_))     => a === b
    case (a @ Custom(_, _), b @ Custom(_, _)) => a === b
    case _                                    => false

  /** @group Optics */
  val builtin: Prism[F2FpuMask, F2Fpu] =
    GenPrism[F2FpuMask, F2FpuMask.Builtin].andThen(Builtin.value)

  /** @group Optics */
  val custom: Prism[F2FpuMask, Custom] =
    GenPrism[F2FpuMask, Custom]

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.flamingos2

import cats.Eq
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.Flamingos2CustomSlitWidth
import lucuma.core.enums.Flamingos2Fpu
import monocle.Focus
import monocle.Iso
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism

sealed trait Flamingos2FpuMask:

  import Flamingos2FpuMask.Imaging
  import Flamingos2FpuMask.Builtin
  import Flamingos2FpuMask.Custom

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

  def builtinFpu: Option[Flamingos2Fpu] =
    builtin.map(_.value)

  def custom: Option[Custom] =
    fold(none, _ => none, _.some)

  def customFilename: Option[NonEmptyString] =
    custom.map(_.filename)

  def customSlitWidth: Option[Flamingos2CustomSlitWidth] =
    custom.map(_.slitWidth)

  def isImaging: Boolean =
    fold(true, _ => false, _ => false)

  def isMOS: Boolean =
    fold(false, _ => false, _ => true)

  def isLongSlit: Boolean =
    fold(false, _ => true, _ => false)

object Flamingos2FpuMask:
  case object Imaging extends Flamingos2FpuMask

  case class Builtin(value: Flamingos2Fpu) extends Flamingos2FpuMask

  object Builtin:
    given Eq[Builtin] =
      Eq.by(_.value)

    /** @group Optics */
    def value: Iso[Builtin, Flamingos2Fpu] =
      Iso[Builtin, Flamingos2Fpu](_.value)(Builtin.apply)

  case class Custom(filename: NonEmptyString, slitWidth: Flamingos2CustomSlitWidth) extends Flamingos2FpuMask

  object Custom:
    given Eq[Custom] = Eq.by(x => (x.filename, x.slitWidth))

    /** @group Optics */
    val filename: Lens[Custom, NonEmptyString] =
      Focus[Custom](_.filename)

    /** @group Optics */
    val customSlitWidth: Lens[Custom, Flamingos2CustomSlitWidth] =
      Focus[Custom](_.slitWidth)

  given Eq[Flamingos2FpuMask] = Eq.instance:
    case (Imaging, Imaging)                   => true
    case (a @ Builtin(_), b @ Builtin(_))     => a === b
    case (a @ Custom(_, _), b @ Custom(_, _)) => a === b
    case _                                    => false

  /** @group Optics */
  val builtin: Prism[Flamingos2FpuMask, Flamingos2Fpu] =
    GenPrism[Flamingos2FpuMask, Flamingos2FpuMask.Builtin].andThen(Builtin.value)

  /** @group Optics */
  val custom: Prism[Flamingos2FpuMask, Custom] =
    GenPrism[Flamingos2FpuMask, Custom]

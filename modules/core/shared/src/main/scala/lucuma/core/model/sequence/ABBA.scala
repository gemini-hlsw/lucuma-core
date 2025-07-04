// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence

import cats.Eq
import cats.derived.*
import cats.syntax.eq.*
import lucuma.core.math.Offset
import monocle.Focus
import monocle.Lens

/**
 * An ABBA pattern containing exactly 4 offsets with symmetric structure.
 */
case class ABBA(a: Offset, b: Offset) derives Eq:
  def all: (Offset, Offset, Offset, Offset) = (a, b, b, a)

object ABBA:

  val Zero: ABBA = ABBA(Offset.Zero, Offset.Zero)

  /**
   * Some(ABBA) if valid, None if the pattern is invalid.
   */
  def apply(first: Offset, second: Offset, third: Offset, fourth: Offset): Option[ABBA] =
    Option.when(first === fourth && second === third):
      ABBA(first, second)

  /**
   * Extract the ABBA pattern.
   */
  def unapply(abba: ABBA): Option[(Offset, Offset, Offset, Offset)] =
    Some((abba.a, abba.b, abba.b, abba.a))

  /** Lens focusing on the a offset (A in A-B-B-A) */
  val a: Lens[ABBA, Offset] = Focus[ABBA](_.a)

  /** Lens focusing on the b offset (B in A-B-B-A) */
  val b: Lens[ABBA, Offset] = Focus[ABBA](_.b)

  /** First offset in the A-B-B-A pattern */
  val first: Lens[ABBA, Offset] = a

  /** Second offset in the A-B-B-A pattern */
  val second: Lens[ABBA, Offset] = b

  /** Third offset in the A-B-B-A pattern */
  val third: Lens[ABBA, Offset] = b

  /** Fourth offset in the A-B-B-A pattern */
  val fourth: Lens[ABBA, Offset] = a

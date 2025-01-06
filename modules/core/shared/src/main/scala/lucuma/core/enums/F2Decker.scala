// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for Flamingos2 decker option.
 * @group Enumerations
 */
sealed abstract class F2Decker(
  val tag: String,
  val shortName: String,
  val longName: String,
  val obsolete: Boolean
) extends Product with Serializable

object F2Decker {

  /** @group Constructors */ case object Imaging extends F2Decker("Imaging", "Imaging", "Imaging", false)
  /** @group Constructors */ case object LongSlit extends F2Decker("LongSlit", "Long Slit", "LongSlit", false)
  /** @group Constructors */ case object MOS extends F2Decker("MOS", "MOS", "MOS", false)

  /** All members of F2Decker, in canonical order. */
  val all: List[F2Decker] =
    List(Imaging, LongSlit, MOS)

  /** Select the member of F2Decker with the given tag, if any. */
  def fromTag(s: String): Option[F2Decker] =
    all.find(_.tag === s)

  /** Select the member of F2Decker with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): F2Decker =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"F2Decker: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  given Enumerated[F2Decker] =
    new Enumerated[F2Decker] {
      def all = F2Decker.all
      def tag(a: F2Decker) = a.tag
      override def unsafeFromTag(s: String): F2Decker =
        F2Decker.unsafeFromTag(s)
    }

}

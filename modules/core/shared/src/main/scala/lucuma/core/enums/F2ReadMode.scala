// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.syntax.eq.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan


/**
 * Enumerated type for Flamingos2 read modes.
 * @group Enumerations (Generated)
 */
sealed abstract class F2ReadMode(
  val tag: String,
  val shortName: String,
  val longName: String,
  val description: String,
  val minimumExposureTime: TimeSpan,
  val recommendedExposureTime: TimeSpan,
  val readoutTime: TimeSpan,
  val readCount: Int,
  val readNoise: Double
) extends Product with Serializable

object F2ReadMode {

  /** @group Constructors */ case object Bright extends F2ReadMode("Bright", "bright", "Bright Object", "Strong Source", 1500.msTimeSpan, 5000.msTimeSpan, 8000.msTimeSpan, 1, 11.7)
  /** @group Constructors */ case object Medium extends F2ReadMode("Medium", "medium", "Medium Object", "Medium Source", 6.secTimeSpan, 21.secTimeSpan, 14.secTimeSpan, 4, 6.0)
  /** @group Constructors */ case object Faint extends F2ReadMode("Faint", "faint", "Faint Object", "Weak Source", 12.secTimeSpan, 85.secTimeSpan, 20.secTimeSpan, 8, 5.0)

  /** All members of F2ReadMode, in canonical order. */
  val all: List[F2ReadMode] =
    List(Bright, Medium, Faint)

  /** Select the member of F2ReadMode with the given tag, if any. */
  def fromTag(s: String): Option[F2ReadMode] =
    all.find(_.tag === s)

  /** Select the member of F2ReadMode with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): F2ReadMode =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"F2ReadMode: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val F2ReadModeEnumerated: Enumerated[F2ReadMode] =
    new Enumerated[F2ReadMode] {
      def all = F2ReadMode.all
      def tag(a: F2ReadMode) = a.tag
      override def unsafeFromTag(s: String): F2ReadMode =
        F2ReadMode.unsafeFromTag(s)
    }

}

// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import cats.syntax.eq._
import lucuma.core.util.Enumerated

/**
 * Enumerated type for Guiding speed
 * @group Enumerations (Generated)
 */
sealed abstract class GuideSpeed(val tag: String)
    extends Product
    with Serializable

object GuideSpeed {

  /** @group Constructors */
  case object Fast extends GuideSpeed("fast")

  /** @group Constructors */
  case object Medium extends GuideSpeed("medium")

  /** @group Constructors */
  case object Slow extends GuideSpeed("slow")

  /** All members of GuideSpeed, in canonical order. */
  val all: List[GuideSpeed] =
    List(Fast, Medium, Slow)

  /** Select the member of GuideSpeed with the given tag, if any. */
  def fromTag(s: String): Option[GuideSpeed] =
    all.find(_.tag === s)

  /** Select the member of GuideSpeed with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GuideSpeed =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GuideSpeed: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GuideProbeEnumerated: Enumerated[GuideSpeed] =
    Enumerated.of[GuideSpeed](Fast, Medium, Slow)

}

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enum
import cats.syntax.eq._
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS amp read mode.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosAmpReadMode(
  val tag: String,
  val shortName: String,
  val longName: String
) extends Product with Serializable

object GmosAmpReadMode {

  /** @group Constructors */ case object Slow extends GmosAmpReadMode("Slow", "slow", "Slow")
  /** @group Constructors */ case object Fast extends GmosAmpReadMode("Fast", "fast", "Fast")

  /** All members of GmosAmpReadMode, in canonical order. */
  val all: List[GmosAmpReadMode] =
    List(Slow, Fast)

  /** Select the member of GmosAmpReadMode with the given tag, if any. */
  def fromTag(s: String): Option[GmosAmpReadMode] =
    all.find(_.tag === s)

  /** Select the member of GmosAmpReadMode with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GmosAmpReadMode =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GmosAmpReadMode: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GmosAmpReadModeEnumerated: Enumerated[GmosAmpReadMode] =
    new Enumerated[GmosAmpReadMode] {
      def all = GmosAmpReadMode.all
      def tag(a: GmosAmpReadMode) = a.tag
      override def unsafeFromTag(s: String): GmosAmpReadMode =
        GmosAmpReadMode.unsafeFromTag(s)
    }

}

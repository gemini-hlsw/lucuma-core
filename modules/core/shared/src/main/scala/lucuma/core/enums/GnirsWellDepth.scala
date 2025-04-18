// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GNRIS Well Depth.
 * @group Enumerations (Generated)
 */
sealed abstract class GnirsWellDepth(
  val tag: String,
  val shortName: String,
  val longName: String,
  val bias_level: Int
) extends Product with Serializable

object GnirsWellDepth {

  /** @group Constructors */ case object Shallow extends GnirsWellDepth("Shallow", "Shallow", "Shallow", 300)
  /** @group Constructors */ case object Deep extends GnirsWellDepth("Deep", "Deep", "Deep", 600)

  /** All members of GnirsWellDepth, in canonical order. */
  val all: List[GnirsWellDepth] =
    List(Shallow, Deep)

  /** Select the member of GnirsWellDepth with the given tag, if any. */
  def fromTag(s: String): Option[GnirsWellDepth] =
    all.find(_.tag === s)

  /** Select the member of GnirsWellDepth with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GnirsWellDepth =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GnirsWellDepth: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GnirsWellDepthEnumerated: Enumerated[GnirsWellDepth] =
    new Enumerated[GnirsWellDepth] {
      def all = GnirsWellDepth.all
      def tag(a: GnirsWellDepth) = a.tag
      override def unsafeFromTag(s: String): GnirsWellDepth =
        GnirsWellDepth.unsafeFromTag(s)
    }

}

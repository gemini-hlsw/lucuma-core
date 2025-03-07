// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS North stage modes.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosNorthStageMode(
  val tag: String,
  val shortName: String,
  val longName: String
) extends Product with Serializable

object GmosNorthStageMode {

  /** @group Constructors */ case object NoFollow extends GmosNorthStageMode("NoFollow", "No Follow", "Do Not Follow")
  /** @group Constructors */ case object FollowXy extends GmosNorthStageMode("FollowXy", "Follow XY", "Follow in XY")

  /** All members of GmosNorthStageMode, in canonical order. */
  val all: List[GmosNorthStageMode] =
    List(NoFollow, FollowXy)

  /** Select the member of GmosNorthStageMode with the given tag, if any. */
  def fromTag(s: String): Option[GmosNorthStageMode] =
    all.find(_.tag === s)

  /** Select the member of GmosNorthStageMode with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GmosNorthStageMode =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GmosNorthStageMode: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GmosNorthStageModeEnumerated: Enumerated[GmosNorthStageMode] =
    new Enumerated[GmosNorthStageMode] {
      def all = GmosNorthStageMode.all
      def tag(a: GmosNorthStageMode) = a.tag
      override def unsafeFromTag(s: String): GmosNorthStageMode =
        GmosNorthStageMode.unsafeFromTag(s)
    }

}

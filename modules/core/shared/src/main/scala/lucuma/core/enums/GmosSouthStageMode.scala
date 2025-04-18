// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq.*
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS South stage mode.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosSouthStageMode(
  val tag: String,
  val shortName: String,
  val longName: String
) extends Product with Serializable

object GmosSouthStageMode {

  /** @group Constructors */ case object NoFollow extends GmosSouthStageMode("NoFollow", "No Follow", "Do Not Follow")
  /** @group Constructors */ case object FollowXyz extends GmosSouthStageMode("FollowXyz", "Follow XYZ", "Follow in XYZ(focus)")
  /** @group Constructors */ case object FollowZ extends GmosSouthStageMode("FollowZ", "Follow Z", "Follow in Z Only")

  /** All members of GmosSouthStageMode, in canonical order. */
  val all: List[GmosSouthStageMode] =
    List(NoFollow, FollowXyz, FollowZ)

  /** Select the member of GmosSouthStageMode with the given tag, if any. */
  def fromTag(s: String): Option[GmosSouthStageMode] =
    all.find(_.tag === s)

  /** Select the member of GmosSouthStageMode with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GmosSouthStageMode =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GmosSouthStageMode: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GmosSouthStageModeEnumerated: Enumerated[GmosSouthStageMode] =
    new Enumerated[GmosSouthStageMode] {
      def all = GmosSouthStageMode.all
      def tag(a: GmosSouthStageMode) = a.tag
      override def unsafeFromTag(s: String): GmosSouthStageMode =
        GmosSouthStageMode.unsafeFromTag(s)
    }

}

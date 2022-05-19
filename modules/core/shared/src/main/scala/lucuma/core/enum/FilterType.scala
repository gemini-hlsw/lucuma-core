// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package `enum`

import cats.syntax.eq._
import lucuma.core.util.Enumerated

/**
 * Enumerated type for filter types.
 * @group Enumerations (Generated)
 */
sealed abstract class FilterType(
  val tag: String
) extends Product with Serializable

object FilterType {

  /** @group Constructors */ case object BroadBand extends FilterType("BroadBand")
  /** @group Constructors */ case object NarrowBand extends FilterType("NarrowBand")
  /** @group Constructors */ case object Combination extends FilterType("Combination")
  /** @group Constructors */ case object Spectroscopic extends FilterType("Spectroscopic")
  /** @group Constructors */ case object Engineering extends FilterType("Engineering")

  /** All members of FilterType, in canonical order. */
  val all: List[FilterType] =
    List(BroadBand, NarrowBand, Combination, Spectroscopic, Engineering)

  /** Select the member of FilterType with the given tag, if any. */
  def fromTag(s: String): Option[FilterType] =
    all.find(_.tag === s)

  /** Select the member of FilterType with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): FilterType =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"FilterType: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val FilterTypeEnumerated: Enumerated[FilterType] =
    new Enumerated[FilterType] {
      def all = FilterType.all
      def tag(a: FilterType) = a.tag
      override def unsafeFromTag(s: String): FilterType =
        FilterType.unsafeFromTag(s)
    }

}

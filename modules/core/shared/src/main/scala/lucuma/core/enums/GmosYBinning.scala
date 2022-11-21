// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq._
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS Y-binning.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosYBinning(
  val tag: String,
  val shortName: String,
  val longName: String,
  val count: Int
) extends Product with Serializable

object GmosYBinning {

  /** @group Constructors */ case object One extends GmosYBinning("One", "1", "One", 1)
  /** @group Constructors */ case object Two extends GmosYBinning("Two", "2", "Two", 2)
  /** @group Constructors */ case object Four extends GmosYBinning("Four", "4", "Four", 4)

  /** All members of GmosYBinning, in canonical order. */
  val all: List[GmosYBinning] =
    List(One, Two, Four)

  /** Select the member of GmosYBinning with the given tag, if any. */
  def fromTag(s: String): Option[GmosYBinning] =
    all.find(_.tag === s)

  /** Select the member of GmosYBinning with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GmosYBinning =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GmosYBinning: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GmosYBinningEnumerated: Enumerated[GmosYBinning] =
    new Enumerated[GmosYBinning] {
      def all = GmosYBinning.all
      def tag(a: GmosYBinning) = a.tag
      override def unsafeFromTag(s: String): GmosYBinning =
        GmosYBinning.unsafeFromTag(s)
    }

}

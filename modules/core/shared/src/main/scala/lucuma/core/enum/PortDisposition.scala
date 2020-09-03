// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enum
import cats.syntax.eq._
import lucuma.core.util.Enumerated

/**
 * Enumerated type for ISS Port Disposition.
 * @group Enumerations (Generated)
 */
sealed abstract class PortDisposition(
  val tag: String,
  val shortName: String
) extends Product with Serializable

object PortDisposition {

  /** @group Constructors */ case object Side extends PortDisposition("Side", "Side Looking")
  /** @group Constructors */ case object Bottom extends PortDisposition("Bottom", "Up Looking")

  /** All members of PortDisposition, in canonical order. */
  val all: List[PortDisposition] =
    List(Side, Bottom)

  /** Select the member of PortDisposition with the given tag, if any. */
  def fromTag(s: String): Option[PortDisposition] =
    all.find(_.tag === s)

  /** Select the member of PortDisposition with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): PortDisposition =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"PortDisposition: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val PortDispositionEnumerated: Enumerated[PortDisposition] =
    new Enumerated[PortDisposition] {
      def all = PortDisposition.all
      def tag(a: PortDisposition) = a.tag
      override def unsafeFromTag(s: String): PortDisposition =
        PortDisposition.unsafeFromTag(s)
    }

}

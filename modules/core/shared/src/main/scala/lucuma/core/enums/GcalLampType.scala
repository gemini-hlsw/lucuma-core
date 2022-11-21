// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq._
import lucuma.core.util.Enumerated

/**
 * Enumerated type for calibration lamp type.
 * @group Enumerations (Generated)
 */
sealed abstract class GcalLampType(
  val tag: String
) extends Product with Serializable

object GcalLampType {

  /** @group Constructors */ case object Arc extends GcalLampType("Arc")
  /** @group Constructors */ case object Flat extends GcalLampType("Flat")

  /** All members of GcalLampType, in canonical order. */
  val all: List[GcalLampType] =
    List(Arc, Flat)

  /** Select the member of GcalLampType with the given tag, if any. */
  def fromTag(s: String): Option[GcalLampType] =
    all.find(_.tag === s)

  /** Select the member of GcalLampType with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GcalLampType =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GcalLampType: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GcalLampTypeEnumerated: Enumerated[GcalLampType] =
    new Enumerated[GcalLampType] {
      def all = GcalLampType.all
      def tag(a: GcalLampType) = a.tag
      override def unsafeFromTag(s: String): GcalLampType =
        GcalLampType.unsafeFromTag(s)
    }

}

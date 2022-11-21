// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq._
import lucuma.core.util.Enumerated

/**
 * Enumerated type for step types.
 * @group Enumerations (Generated)
 */
sealed abstract class StepType(
  val tag: String
) extends Product with Serializable

object StepType {

  /** @group Constructors */ case object Bias extends StepType("Bias")
  /** @group Constructors */ case object Dark extends StepType("Dark")
  /** @group Constructors */ case object Gcal extends StepType("Gcal")
  /** @group Constructors */ case object Science extends StepType("Science")
  /** @group Constructors */ case object SmartGcal extends StepType("SmartGcal")

  /** All members of StepType, in canonical order. */
  val all: List[StepType] =
    List(Bias, Dark, Gcal, Science, SmartGcal)

  /** Select the member of StepType with the given tag, if any. */
  def fromTag(s: String): Option[StepType] =
    all.find(_.tag === s)

  /** Select the member of StepType with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): StepType =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"StepType: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val StepTypeEnumerated: Enumerated[StepType] =
    new Enumerated[StepType] {
      def all = StepType.all
      def tag(a: StepType) = a.tag
      override def unsafeFromTag(s: String): StepType =
        StepType.unsafeFromTag(s)
    }

}

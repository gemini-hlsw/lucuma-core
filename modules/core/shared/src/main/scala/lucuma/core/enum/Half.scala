// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package `enum`
import cats.syntax.eq._
import lucuma.core.util.Enumerated

import java.time.Month
import java.time.Month._

/**
 * Enumerated type for semester half.
 * @group Enumerations (Generated)
 */
sealed abstract class Half(
  val tag:   String,
  val toInt: Int
) extends Product
    with Serializable {
  def startMonth: Month
  def endMonth: Month
}

object Half {

  /** @group Constructors */
  case object A extends Half("A", 0) {
    val startMonth: Month = FEBRUARY
    val endMonth: Month   = JULY
  }

  /** @group Constructors */
  case object B extends Half("B", 1) {
    val startMonth: Month = AUGUST
    val endMonth: Month   = JANUARY
  }

  /** All members of Half, in canonical order. */
  val all: List[Half] =
    List(A, B)

  /** Select the member of Half with the given tag, if any. */
  def fromTag(s: String): Option[Half] =
    all.find(_.tag === s)

  /** Select the member of Half with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): Half =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"Half: Invalid tag: '$s'"))

  def unsafeFromInt(n: Int): Half =
    fromInt(n).getOrElse(throw new NoSuchElementException(n.toString))

  def fromInt(n: Int): Option[Half] =
    all.find(_.toInt === n)

  def fromMonth(m: Month): Half =
    m match {
      case FEBRUARY | MARCH | APRIL | MAY | JUNE | JULY                 => A
      case AUGUST | SEPTEMBER | OCTOBER | NOVEMBER | DECEMBER | JANUARY => B
    }

  /** @group Typeclass Instances */
  implicit val HalfEnumerated: Enumerated[Half] =
    new Enumerated[Half] {
      def all = Half.all
      def tag(a:                    Half)         = a.tag
      override def unsafeFromTag(s: String): Half =
        Half.unsafeFromTag(s)
    }

}

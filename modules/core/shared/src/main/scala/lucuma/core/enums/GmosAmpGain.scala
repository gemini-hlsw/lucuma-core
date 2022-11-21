// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums
import cats.syntax.eq._
import lucuma.core.util.Enumerated

/**
 * Enumerated type for GMOS amp gain.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosAmpGain(
  val tag: String,
  val shortName: String,
  val longName: String
) extends Product with Serializable

object GmosAmpGain {

  /** @group Constructors */ case object Low extends GmosAmpGain("Low", "Low", "Low")
  /** @group Constructors */ case object High extends GmosAmpGain("High", "High", "High")

  /** All members of GmosAmpGain, in canonical order. */
  val all: List[GmosAmpGain] =
    List(Low, High)

  /** Select the member of GmosAmpGain with the given tag, if any. */
  def fromTag(s: String): Option[GmosAmpGain] =
    all.find(_.tag === s)

  /** Select the member of GmosAmpGain with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GmosAmpGain =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GmosAmpGain: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GmosAmpGainEnumerated: Enumerated[GmosAmpGain] =
    new Enumerated[GmosAmpGain] {
      def all = GmosAmpGain.all
      def tag(a: GmosAmpGain) = a.tag
      override def unsafeFromTag(s: String): GmosAmpGain =
        GmosAmpGain.unsafeFromTag(s)
    }

}

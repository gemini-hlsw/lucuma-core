// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package enums

import cats.syntax.eq.*
import lucuma.core.math.Angle
import lucuma.core.math.Declination
import lucuma.core.optics.syntax.prism.*
import lucuma.core.util.Enumerated

/**
 * Definition for how the range from sunset to sunrise should be defined for
 * a night. There are various standard options for definition where the night
 * begins and ends which are represented as static constants in side this
 * class.
 */
sealed abstract class TwilightType(val tag: String, val horizonAngle: Declination)
    extends Product
    with Serializable

object TwilightType {
  case object Official
      extends TwilightType(
        "Official",
        Declination.fromAngle.unsafeGet(Angle.fromDoubleDegrees(50.0 / 60.0))
      )
  case object Civil
      extends TwilightType("Civil", Declination.fromAngle.unsafeGet(Angle.fromDoubleDegrees(6.0)))
  case object Nautical
      extends TwilightType("Nautical",
                           Declination.fromAngle.unsafeGet(Angle.fromDoubleDegrees(12.0))
      )
  case object Astronomical
      extends TwilightType("Astronomical",
                           Declination.fromAngle.unsafeGet(Angle.fromDoubleDegrees(18.0))
      )

  /** All members of TwilightType, in canonical order. */
  val all: List[TwilightType] = List(Official, Civil, Nautical, Astronomical)

  /** Select the member of TwilightType with the given tag, if any. */
  def fromTag(s: String): Option[TwilightType] =
    all.find(_.tag === s)

  /** Select the member of TwilightType with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): TwilightType =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"TwilightType: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val enumeratedTwilightType: Enumerated[TwilightType] =
    new Enumerated[TwilightType] {
      def all = TwilightType.all
      def tag(a: TwilightType) = a.tag
    }
}

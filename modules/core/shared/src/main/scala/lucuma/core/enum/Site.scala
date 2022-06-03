// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core
package `enum`
import cats.syntax.eq._
import coulomb.*
import coulomb.syntax.*
import coulomb.units.si.Meter
import eu.timepit.refined.numeric._
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.math.Angle
import lucuma.core.math.Lat
import lucuma.core.math.Lon
import lucuma.core.math.Place
import lucuma.core.math.units._
import lucuma.core.util.Enumerated

import java.time.ZoneId

/**
  * Enumerated type for Gemini observing sites.
  * @group Enumerations (Generated)
  */
sealed abstract class Site(
  val tag:       String,
  val shortName: String,
  val longName:  String,
  val place:     Place,
  val mountain:  String
) extends Product
    with Serializable {
  val latitude: Lat                        = place.latitude
  val longitude: Lon                       = place.longitude
  val altitude: Quantity[NonNegInt, Meter] = place.altitude
  val timezone: ZoneId                     = place.timezone
}

object Site {

  /** @group Constructors */
  case object GN
      extends Site("GN",
                   "GN",
                   "Gemini North",
                   Place(
                     Lat.fromAngleWithCarry(Angle.fromDoubleDegrees(19.8238068))._1,
                     Lon.fromDoubleDegrees(-155.4690550),
                     4213.withRefinedUnit[NonNegative, Meter],
                     ZoneId.of("Pacific/Honolulu")
                   ),
                   "Mauna Kea"
      )

  /** @group Constructors */
  case object GS
      extends Site("GS",
                   "GS",
                   "Gemini South",
                   Place(
                     Lat.fromAngleWithCarry(Angle.fromDoubleDegrees(-30.2407494))._1,
                     Lon.fromDoubleDegrees(-70.7366867),
                     2722.withRefinedUnit[NonNegative, Meter],
                     ZoneId.of("America/Santiago")
                   ),
                   "Cerro Pachon"
      )

  /** All members of Site, in canonical order. */
  val all: List[Site] =
    List(GN, GS)

  /** Select the member of Site with the given tag, if any. */
  def fromTag(s: String): Option[Site] =
    all.find(_.tag === s)

  /** Select the member of Site with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): Site =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"Site: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val SiteEnumerated: Enumerated[Site] =
    new Enumerated[Site] {
      def all = Site.all
      def tag(a:                    Site)         = a.tag
      override def unsafeFromTag(s: String): Site =
        Site.unsafeFromTag(s)
    }

}

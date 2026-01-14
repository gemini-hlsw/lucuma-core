// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Order
import cats.Show
import cats.implicits.*
import io.circe.*
import lucuma.core.data.PerSite
import lucuma.core.enums.EphemerisKeyType
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.EphemerisCoordinates
import lucuma.core.model.EphemerisTracking
import lucuma.core.model.parser.EphemerisKeyParsers
import lucuma.core.optics.Format
import lucuma.core.syntax.string.*
import lucuma.core.util.Timestamp
import monocle.Focus
import monocle.Lens

import java.time.Instant

sealed trait Ephemeris[E <: Ephemeris.Element]:
  def key: Ephemeris.Key
  def elements: PerSite[List[E]]
  def toEphemerisTracking(site: Site): EphemerisTracking =
    EphemerisTracking.fromList:
      elements(site).flatMap: e =>
        Timestamp.fromInstant(e.when).tupleRight(EphemerisCoordinates(e.coordinates, e.velocity))

object Ephemeris:

  sealed trait Element:
    def when: Instant
    def coordinates: Coordinates
    def velocity: Offset
    def toEphemerisCoordinates: EphemerisCoordinates =
      EphemerisCoordinates(coordinates, velocity)

  final case class UserSupplied(
    key: Key.UserSupplied,
    elements: PerSite[List[UserSupplied.Element]]
  ) extends Ephemeris[UserSupplied.Element]

  object UserSupplied:
    final case class Element(
      when: Instant,
      coordinates: Coordinates,
      velocity: Offset,
    ) extends Ephemeris.Element

  final case class Horizons(
    key: Key.Horizons,
    start: Instant,
    stop: Instant,
    elements: PerSite[List[Horizons.Element]]
  ) extends Ephemeris[Horizons.Element]

  object Horizons:
    final case class Element(
      when: Instant,
      coordinates: Coordinates,
      velocity: Offset,
      airmass: Option[AirMass],
      extinction: Option[Extinction],
      visualMagnitude: Double,
      surfaceBrightness: Option[Double],
    ) extends Ephemeris.Element

  /**
   * Ephemeris data lookup key which uniquely identifies a non-sidereal object in
   * the database.
   */
  sealed trait Key extends Product with Serializable {

    /**
     * Human readable desgination that discriminates among ephemeris keys of the
     * same type.
     */
    def des: String

    /**
     * Extracts the ephemeris lookup key type.
     */
    def keyType: EphemerisKeyType =
      this match {
        case Key.AsteroidNew(_)  => EphemerisKeyType.AsteroidNew
        case Key.AsteroidOld(_)  => EphemerisKeyType.AsteroidOld
        case Key.Comet(_)        => EphemerisKeyType.Comet
        case Key.MajorBody(_)    => EphemerisKeyType.MajorBody
        case Key.UserSupplied(_) => EphemerisKeyType.UserSupplied
      }

  }

  object Key extends EphemerisOptics with EphemerisJson {

    /**
     * Unique Horizons designation, which should allow for reproducible ephemeris
     * queries <b>if</b> the values passed to the constructors are extracted
     * correctly from search results.
     */
    sealed abstract class Horizons(val queryString: String) extends Key

    /**
     * Horizons designation for asteroids, old and new style.
     */
    sealed abstract class Asteroid(s: String) extends Horizons(s)

    /**
     * Designation for a comet, in the current apparition. Example: `C/1973 E1`
     * for Kohoutek, yielding the query string `NAME=C/1973 E1;CAP`.
     */
    final case class Comet(des: String) extends Horizons(s"NAME=$des;CAP")

    object Comet {
      val des: Lens[Comet, String] = Focus[Comet](_.des)
    }

    /**
     * Designation for an asteroid under modern naming conventions. Example:
     * `1971 UC1` for 1896 Beer, yielding a query string `ASTNAM=1971 UC1`.
     */
    final case class AsteroidNew(des: String) extends Asteroid(s"ASTNAM=$des")

    object AsteroidNew {
      val des: Lens[AsteroidNew, String] = Focus[AsteroidNew](_.des)
    }

    /**
     * Designation for an asteroid under "old" naming conventions. These are
     * small numbers. Example: `4` for Vesta, yielding a query string `4;`
     */
    final case class AsteroidOld(num: Int) extends Asteroid(s"$num;") {
      override def des: String =
        num.toString
    }

    object AsteroidOld {
      val num: Lens[AsteroidOld, Int] = Focus[AsteroidOld](_.num)
    }

    /**
     * Designation for a major body (planet or satellite thereof). These have
     * small numbers. Example: `606` for Titan, yielding a query string `606`.
     */
    final case class MajorBody(num: Int) extends Horizons(s"$num") {
      override def des: String =
        num.toString
    }
    object MajorBody {
      val num: Lens[MajorBody, Int] = Focus[MajorBody](_.num)
    }

    /**
     * Identifies a user-supplied collection of ephemeris data, where the number
     * comes from a database sequence.
     */
    final case class UserSupplied(id: Long) extends Key {
      override def des: String =
        id.toString
    }

    object UserSupplied {
      val id: Lens[UserSupplied, Long] = Focus[UserSupplied](_.id)
    }

    given Show[Key] =
      Show.fromToString

    given Order[Key] =
      Order.by(fromString.reverseGet)
  }

  trait EphemerisOptics { this: Key.type =>

    val fromString: Format[String, Key] =
      Format(
        EphemerisKeyParsers.ephemerisKey.parseAll(_).toOption,
        k => {
          val (keyType, des) = fromTypeAndDes.reverseGet(k)
          s"${keyType.tag}_${des}"
        }
      )

    val fromTypeAndDes: Format[(EphemerisKeyType, String), Key] =
      Format(
        { case (t, des) =>
          t match {
            case EphemerisKeyType.Comet        => Some(Key.Comet(des))
            case EphemerisKeyType.AsteroidNew  => Some(Key.AsteroidNew(des))
            case EphemerisKeyType.AsteroidOld  => des.parseIntOption.map(Key.AsteroidOld(_))
            case EphemerisKeyType.MajorBody    => des.parseIntOption.map(Key.MajorBody(_))
            case EphemerisKeyType.UserSupplied => des.parseIntOption.map(Key.UserSupplied(_))
          }
        },
        k => (k.keyType, k.des)
      )
  }

  trait EphemerisJson { this: EphemerisOptics & Key.type =>
    given Encoder[Key] =
      Encoder[String].contramap(fromString.reverseGet)

    given Decoder[Key] =
      Decoder[String].emap(s => fromString.getOption(s).toRight(s"Invalid Key value: [$s]"))
  }

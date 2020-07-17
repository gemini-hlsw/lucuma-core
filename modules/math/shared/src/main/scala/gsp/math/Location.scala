// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gsp.math

import cats.Eq
import cats.Show
import monocle.Lens
import monocle.macros.GenLens

/** A point on Earth, given latitude, longitude and altitude in m above sea level. */
final case class Location(latitude: Lat, longitude: Lon, altitude: Double)

object Location {

  /** @group Typeclass Instances */
  implicit val LocationEqual: Eq[Location] = Eq.fromUniversalEquals

  /** @group Typeclass Instances */
  implicit val LocationShow: Show[Location] = Show.fromToString

  /** @group Optics */
  val latitude: Lens[Location, Lat] =
    GenLens[Location](_.latitude)

  /** @group Optics */
  val longitude: Lens[Location, Lon] =
    GenLens[Location](_.longitude)

  /** @group Optics */
  val altitude: Lens[Location, Double] =
    GenLens[Location](_.altitude)
}

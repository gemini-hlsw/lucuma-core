// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma
package core

/** Mathematical data types for general use, not specific to the Gem model. */
package object math {

  type RA = RightAscension
  val RA: RightAscension.type = RightAscension

  type Dec = Declination
  val Dec: Declination.type = Declination

  type Lat = Declination
  val Lat: Declination.type = Declination

  type Lon = Angle
  val Lon: Angle.type = Angle

}

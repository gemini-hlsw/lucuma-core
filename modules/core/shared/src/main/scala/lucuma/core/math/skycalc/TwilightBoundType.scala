// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math.skycalc

import cats._

/**
  * Definition for how the range from sunset to sunrise should be defined for
  * a night.  There are various standard options for definition where the night
  * begins and ends which are represented as static constants in side this
  * class.
  */
sealed abstract class TwilightBoundType(val name: String, val horizonAngle: Double)

object TwilightBoundType {
  case object Official     extends TwilightBoundType("Official", 50.0 / 60.0)
  case object Civil        extends TwilightBoundType("Civil", 6.0)
  case object Nautical     extends TwilightBoundType("Nautical", 12.0)
  case object Astronomical extends TwilightBoundType("Astronomical", 18.0)

  val all: List[TwilightBoundType] = List(Official, Civil, Nautical, Astronomical)

  // Hashed index lookup, for efficient use as an `Order`.
  private lazy val indexOfTag: Map[TwilightBoundType, Int] = all.zipWithIndex.toMap

  /** @group Typeclass Instances */
  implicit val TwilightBoundTypeOrder: Order[TwilightBoundType] = Order.by(indexOfTag)

  /** @group Typeclass Instances */
  implicit val TwilightBoundTypeShow: Show[TwilightBoundType] = Show.show(_.name)
}

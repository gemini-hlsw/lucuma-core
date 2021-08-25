// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Order
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.auto._
import eu.timepit.refined.cats._
import lucuma.core.util.Display

final case class CloudExtinction(val toDeciMagnitudes: NonNegInt) extends Product with Serializable {
  def toMagnitudes: Double = toDeciMagnitudes / 10.0
  def label: String        = f"< $toMagnitudes%.1f mag"
}

object CloudExtinction {
  val PointOne: CloudExtinction       = CloudExtinction(1)
  val PointThree: CloudExtinction     = CloudExtinction(3)
  val PointFive: CloudExtinction      = CloudExtinction(5)
  val OnePointZero: CloudExtinction   = CloudExtinction(10)
  val OnePointFive: CloudExtinction   = CloudExtinction(15)
  val TwoPointZero: CloudExtinction   = CloudExtinction(20)
  val ThreePointZero: CloudExtinction = CloudExtinction(30)

  val all: List[CloudExtinction] = List(
    PointOne,
    PointThree,
    PointFive,
    OnePointZero,
    OnePointFive,
    TwoPointZero,
    ThreePointZero
  )

  /**
    * @group typeclass
    */
  implicit val CloudExtinctionOrder: Order[CloudExtinction] =
    Order.by(_.toDeciMagnitudes)

  implicit val CloudExtinctionDisplay: Display[CloudExtinction] =
    Display.byShortName(_.label)
}

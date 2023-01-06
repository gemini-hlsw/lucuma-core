// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Eq
import cats.syntax.order._
import coulomb.*
import coulomb.syntax.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.units._

/** Wavelength coverage. */
sealed trait Coverage {
  import Coverage.{ Empty, Range }

  /** Intersect this `Coverage` with another. */
  @scala.annotation.nowarn // TODO
  def ⋂(other: Coverage): Coverage =
    (this, other) match {
      case (Empty, _) => Empty
      case (_, Empty) => Empty
      case (Range(a, b), Range(aʹ, bʹ)) => Coverage(a max aʹ, b min bʹ)
    }

  /** Coverage width; i.e., difference between max and min (or zero). */
  def width: Quantity[NonNegInt, Picometer] =
    this match {
      case Empty       => NonNegInt.unsafeFrom(0).withUnit[Picometer]
      case Range(a, b) => NonNegInt.unsafeFrom(b.toPicometers.value.value - a.toPicometers.value.value).withUnit[Picometer]
    }

  /** Range projection; defined when non-empty. */
  def range: Option[Coverage.Range] =
    this match {
      case Empty           => None
      case r @ Range(_, _) => Some(r)
    }

}

object Coverage {

   /** The empty `Coverage` with no bounds and a width of zero. */
  case object Empty extends Coverage

  /** Non-empty `Coverage` with upper and lower bounds. */
  sealed abstract case class Range(min: Wavelength, max: Wavelength) extends Coverage {
    require(min < max) // smart ctor should guarantee this
  }

  @scala.annotation.nowarn // TODO
  implicit val EqCoverage: Eq[Coverage] =
    Eq.instance {
      case (Empty, Empty)                         => true
      case (Range(aMin, aMax), Range(bMin, bMax)) => aMin === bMin && aMax === bMax
      case _                                      => false
    }

  /** Construct a `Coverage`, empty if `min >= max`. */
  def apply(min: Wavelength, max: Wavelength): Coverage =
    if (min < max) new Range(min, max) {} else Empty

    /** Construct a `Coverage` centered at the given wavelength, with the specified width. */
  def centered(central: Wavelength, width: Quantity[PosInt, Picometer]): Coverage = {
    // I believe it is possible to do this directly without dropping down to
    // ints, with a magic, un-discoverable combination of imports.
    val centPm = central.toPicometers.value.value
    val halfPm = width.value.value / 2
    val min    = Wavelength.unsafeFromIntPicometers(centPm - halfPm)
    val max    = Wavelength.unsafeFromIntPicometers(centPm + halfPm)
    apply(min, max)
  }

}
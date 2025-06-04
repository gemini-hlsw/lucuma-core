// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.kernel.Order
import monocle.Focus
import monocle.Lens
import monocle.Optional

/** A quasirectangular region on a sphere, given by ranges in RA and Dec. */
final case class Region(raArc: Arc[RightAscension], decArc: Arc[Declination]) {

  def contains(coords: Coordinates): Boolean =
    raArc.contains(coords.ra) &&
    decArc.contains(coords.dec)

  def containsAll(other: Region): Boolean =
    raArc.containsAll(other.raArc) && 
    decArc.containsAll(other.decArc)

  def existsOverlap(other: Region): Boolean =
    raArc.existsOverlap(other.raArc) &&
    decArc.existsOverlap(other.decArc)

  def isEmpty: Boolean =
    raArc.isEmpty || decArc.isEmpty

  def nonEmpty: Boolean =
    !isEmpty

  def isFull: Boolean = 
    raArc.isFull && decArc.isFull

  def nonFull: Boolean =
    !isFull

}

object Region extends RegionOptics {

  val Empty = Region(Arc.Empty(), Arc.Empty())
  val Full  = Region(Arc.Full(), Arc.Full())

  given Order[Region] =
    Order.by: r => 
      (r.raArc, r.decArc)

}

trait RegionOptics {

  val raArc: Lens[Region, Arc[RightAscension]] =
    Focus[Region](_.raArc)

  val raArcStart: Optional[Region, RightAscension] =
    raArc.andThen(Arc.start[RightAscension])
  
  val raArcEnd: Optional[Region, RightAscension] =
    raArc.andThen(Arc.end[RightAscension])
    
  val decArc: Lens[Region, Arc[Declination]] =
    Focus[Region](_.decArc)
  
  val decArcStart: Optional[Region, Declination] =
    decArc.andThen(Arc.start[Declination])
  
  val decArcEnd: Optional[Region, Declination] =
    decArc.andThen(Arc.end[Declination])
  
}
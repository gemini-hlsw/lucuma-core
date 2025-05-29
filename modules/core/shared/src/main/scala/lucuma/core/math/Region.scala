// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.Eq
import monocle.Focus
import monocle.Lens

/** A quasirectangular region on a sphere, given by ranges in RA and Dec. */
final case class Region(raArc: Arc[RightAscension], decArc: Arc[Declination]):

  def containsAll(other: Region): Boolean =
    raArc.containsAll(other.raArc) && 
    decArc.containsAll(other.decArc)

  def contains(coords: Coordinates): Boolean =
    raArc.contains(coords.ra) &&
    decArc.contains(coords.dec)

  def existsOverlap(other: Region): Boolean =
    raArc.existsOverlap(other.raArc) &&
    decArc.existsOverlap(other.decArc)
  
object Region extends RegionOptics:

  given Eq[Region] =
    Eq.by(a => (a.raArc, a.decArc))

trait RegionOptics:

  val raArc: Lens[Region, Arc[RightAscension]] =
    Focus[Region](_.raArc)

  // val raArcStart: Lens[Region, RightAscension] =
  //   raArc.andThen(Arc.start[RightAscension])
  
  // val raArcEnd: Lens[Region, RightAscension] =
  //   raArc.andThen(Arc.end[RightAscension])
  
  // val decArc: Lens[Region, Arc[Declination]] =
  //   Focus[Region](_.decArc)
  
  // val decArcStart: Lens[Region, Declination] =
  //   decArc.andThen(Arc.start[Declination])
  
  // val decArcEnd: Lens[Region, Declination] =
  //   decArc.andThen(Arc.end[Declination])
    
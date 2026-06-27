// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gnirs

import cats.Eq
import cats.derived.*
import lucuma.core.enums.GnirsFpuIfu
import lucuma.core.enums.GnirsFpuOther
import lucuma.core.enums.GnirsFpuSlit
import monocle.Iso
import monocle.Prism
import monocle.macros.GenPrism

/**
 * The GNIRS focal plane unit. It is either a spectroscopy aperture — a long-slit
 * (`GnirsFpuSlit`) or an IFU (`GnirsFpuIfu`) — or one of the other apertures such
 * as the acquisition mirror, pupil viewer or pinholes (`GnirsFpuOther`).
 */
sealed trait GnirsFpu derives Eq:

  /** Folds over the three leaf aperture kinds. */
  def fold[A](fs: GnirsFpuSlit => A, fi: GnirsFpuIfu => A, fo: GnirsFpuOther => A): A =
    this match
      case GnirsFpu.Spectroscopy.Slit(v) => fs(v)
      case GnirsFpu.Spectroscopy.Ifu(v)  => fi(v)
      case GnirsFpu.Other(v)             => fo(v)

object GnirsFpu:

  /** Apertures that disperse light to the detector: a long slit or an IFU. */
  sealed trait Spectroscopy extends GnirsFpu derives Eq

  object Spectroscopy:
    case class Slit(value: GnirsFpuSlit) extends Spectroscopy
    case class Ifu(value: GnirsFpuIfu)   extends Spectroscopy

    val slit: Prism[Spectroscopy, GnirsFpuSlit] =
      GenPrism[Spectroscopy, Slit].andThen(Iso[Slit, GnirsFpuSlit](_.value)(Slit(_)))

    val ifu: Prism[Spectroscopy, GnirsFpuIfu] =
      GenPrism[Spectroscopy, Ifu].andThen(Iso[Ifu, GnirsFpuIfu](_.value)(Ifu(_)))

  /** Non-spectroscopy apertures: acquisition mirror, pupil viewer, pinholes. */
  case class Other(value: GnirsFpuOther) extends GnirsFpu

  val spectroscopy: Prism[GnirsFpu, Spectroscopy] =
    GenPrism[GnirsFpu, Spectroscopy]

  val other: Prism[GnirsFpu, GnirsFpuOther] =
    GenPrism[GnirsFpu, Other].andThen(Iso[Other, GnirsFpuOther](_.value)(Other(_)))

  // The leaf prisms reach through the spectroscopy group.
  val slit: Prism[GnirsFpu, GnirsFpuSlit] =
    spectroscopy.andThen(Spectroscopy.slit)

  val ifu: Prism[GnirsFpu, GnirsFpuIfu] =
    spectroscopy.andThen(Spectroscopy.ifu)

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gnirs

import cats.Eq
import cats.derived.*
import lucuma.core.enums.GnirsFpuOther
import lucuma.core.enums.GnirsFpuSlit
import monocle.Iso
import monocle.Prism
import monocle.macros.GenPrism

/**
 * The GNIRS focal plane unit, which is either a long-slit aperture
 * (`GnirsFpuSlit`) or one of the other apertures such as the acquisition mirror,
 * pupil viewer or pinholes (`GnirsFpuOther`).
 */
enum GnirsFpu derives Eq:
  case Slit(value: GnirsFpuSlit)
  case Other(value: GnirsFpuOther)

  def fold[A](fs: GnirsFpuSlit => A, fo: GnirsFpuOther => A): A =
    this match
      case Slit(v)  => fs(v)
      case Other(v) => fo(v)

object GnirsFpu:
  val slit: Prism[GnirsFpu, GnirsFpuSlit] =
    GenPrism[GnirsFpu, Slit].andThen(Iso[Slit, GnirsFpuSlit](_.value)(Slit(_)))

  val other: Prism[GnirsFpu, GnirsFpuOther] =
    GenPrism[GnirsFpu, Other].andThen(Iso[Other, GnirsFpuOther](_.value)(Other(_)))

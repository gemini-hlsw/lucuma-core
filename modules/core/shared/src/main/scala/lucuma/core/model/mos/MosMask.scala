// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.mos

import cats.Eq
import cats.Show
import cats.syntax.eq.*
import lucuma.core.enums.MosSlitPriority
import monocle.Focus
import monocle.Lens

/**
 * A complete multi-object spectroscopy mask design: the metadata of the design together with every
 * slit it places.
 *
 * This is the whole-file view. Consumers that process slits one at a time do not need it — a
 * decoded [[MosMaskSlit]] is self describing, because the parser has already resolved the
 * instrument's axis convention into physical slit width and length.
 *
 * The same design is known by two names in Gemini's workflow. It is an ''object definition file''
 * as submitted by a principal investigator, and a ''mask definition file'' once converted for the
 * mask cutting machine. Both are read as this one type.
 */
case class MosMask(
  header: MosMaskHeader,
  slits:  List[MosMaskSlit]
):

  /**
   * Slits targeting acquisition stars, used to align the mask on sky.
   *
   * A usable design needs at least two, and three is preferred.
   */
  def acquisitionSlits: List[MosMaskSlit] =
    slits.filter(_.isAcquisition)

  /** Slits targeting science objects rather than acquisition stars. */
  def scienceSlits: List[MosMaskSlit] =
    slits.filterNot(_.isAcquisition)

  /** Slits at the given placement priority. */
  def slitsWithPriority(priority: MosSlitPriority): List[MosMaskSlit] =
    slits.filter(_.priority === priority)

object MosMask:

  given Eq[MosMask] =
    Eq.by(m => (m.header, m.slits))

  given Show[MosMask] =
    Show.fromToString

  /** @group Optics */
  val header: Lens[MosMask, MosMaskHeader] =
    Focus[MosMask](_.header)

  /** @group Optics */
  val slits: Lens[MosMask, List[MosMaskSlit]] =
    Focus[MosMask](_.slits)

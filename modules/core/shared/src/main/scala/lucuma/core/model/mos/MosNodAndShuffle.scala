// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.mos

import cats.Eq
import cats.Show
import cats.derived.*
import lucuma.core.math.Angle
import monocle.Prism
import monocle.macros.GenPrism

/**
 * Nod & Shuffle configuration recorded in a MOS mask design.
 *
 * Nod & Shuffle constrains the design itself, not just the observation, so these parameters travel
 * with the mask. The two shuffling modes impose different constraints and carry different
 * parameters, which is why they are modelled as distinct cases rather than as loose optional
 * fields — in a mask file the corresponding keywords only ever appear in these fixed groups.
 *
 * Shuffle distances and band geometry are given in ''unbinned'' pixels.
 */
enum MosNodAndShuffle derives Eq, Show:

  /** Not a Nod & Shuffle mask. */
  case None

  /**
   * Every science slit shares one length, and the design is shuffled by a fixed distance.
   *
   * @param shuffleDistance charge shuffle distance, in unbinned pixels
   * @param binning         detector binning the design assumes
   * @param slitLength      the length every science slit is forced to
   */
  case MicroShuffle(
    shuffleDistance: Int,
    binning:         Int,
    slitLength:      Angle
  )

  /**
   * Slits are confined to science bands of a fixed height.
   *
   * @param shuffleDistance charge shuffle distance, in unbinned pixels
   * @param binning         detector binning the design assumes
   * @param bandSize        height of a science band, in unbinned pixels
   * @param bandOffset      offset of the band pattern, in unbinned pixels
   */
  case BandShuffle(
    shuffleDistance: Int,
    binning:         Int,
    bandSize:        Int,
    bandOffset:      Int
  )

object MosNodAndShuffle:

  /** @group Optics */
  val none: Prism[MosNodAndShuffle, MosNodAndShuffle.None.type] =
    GenPrism[MosNodAndShuffle, MosNodAndShuffle.None.type]

  /** @group Optics */
  val microShuffle: Prism[MosNodAndShuffle, MosNodAndShuffle.MicroShuffle] =
    GenPrism[MosNodAndShuffle, MosNodAndShuffle.MicroShuffle]

  /** @group Optics */
  val bandShuffle: Prism[MosNodAndShuffle, MosNodAndShuffle.BandShuffle] =
    GenPrism[MosNodAndShuffle, MosNodAndShuffle.BandShuffle]

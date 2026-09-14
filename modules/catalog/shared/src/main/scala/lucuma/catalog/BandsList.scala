// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.Eq
import cats.derived.*
import lucuma.core.enums.Band

/**
 * Defines a list of bands It is used, e.g. to extract a magnitude from a target
 */
sealed trait BandsList derives Eq:
  def bands: List[Band]

  /**
   * union operation
   */
  def ∪(that: BandsList): BandsList

object BandsList:

  /**
   * Extracts the first valid Gaia Band Magnitude if available
   */
  case object GaiaBandsList extends BandsList:
    val bands = List(Band.GaiaRP, Band.Gaia, Band.GaiaBP) // Order is important

    def ∪(that: BandsList): BandsList =
      that match
        case GaiaBandsList     => this
        case RBandsList        => GaiaAndRBandsList
        case GaiaAndRBandsList => GaiaAndRBandsList

  /**
   * Johnson-Cousins R, the band Altair guide star limits are given in. Gaia has no R column, so
   * catalog queries widen a G constraint (see `GaiaPhotometry`) and consumers estimate R from G, BP
   * and RP.
   */
  case object RBandsList extends BandsList:
    val bands: List[Band] = List(Band.R)

    def ∪(that: BandsList): BandsList =
      that match
        case RBandsList        => this
        case GaiaBandsList     => GaiaAndRBandsList
        case GaiaAndRBandsList => GaiaAndRBandsList

  /** Union of the Gaia and R lists, only produced by `∪`. */
  private case object GaiaAndRBandsList extends BandsList:
    val bands: List[Band] = RBandsList.bands ++ GaiaBandsList.bands

    def ∪(that: BandsList): BandsList = this

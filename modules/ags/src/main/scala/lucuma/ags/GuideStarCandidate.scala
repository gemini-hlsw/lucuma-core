// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import lucuma.catalog.BandsList
import lucuma.catalog.GaiaPhotometry
import lucuma.catalog.GuideStarCatalogResult
import lucuma.core.enums.Band
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Epoch
import lucuma.core.math.dimensional.Units
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import monocle.Focus
import monocle.Lens

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneId
import scala.collection.immutable.SortedMap

// The only bands a candidate keeps, those AGS reads: R for the Altair AOWFS, the Gaia ones elsewhere.
val CandidateBands: List[Band] =
  List(Band.R, Band.Gaia, Band.GaiaBP, Band.GaiaRP)

/**
 * Poors' man Target.Sidereal: tracking plus the catalog's bare brightness values, no units, errors
 * or metadata. R for Altair is not stored: it is estimated from the Gaia bands on first use and
 * memoized, so a cached candidate holds catalog data only. RUWE, when known, ranks otherwise equal
 * candidates.
 */
case class GuideStarCandidate(
  id:           GuideStarId,
  tracking:     SiderealTracking,
  brightnesses: SortedMap[Band, BrightnessValue],
  ruwe:         Option[BigDecimal] = None
) derives Eq {

  def catalogName: Option[GuideStarName] = id.catalogName

  /** Catalog R if the star has one, else the estimate from G, BP and RP. */
  lazy val rBrightness: Option[BrightnessValue] =
    brightnesses.get(Band.R).orElse(GaiaPhotometry.estimatedR(brightnesses))

  /** The first brightness available among the bands a probe works in, in that order. */
  def brightnessIn(bands: BandsList): Option[(Band, BrightnessValue)] =
    bands.bands.collectFirstSome: band =>
      val brightness: Option[BrightnessValue] =
        if band === Band.R then rBrightness else brightnesses.get(band)
      brightness.tupleLeft(band)

  // Reset the candidate to a given instant
  // This can be used to calculate and cache the location base on proper motion
  // The tracking variables are reset to match the epoch to the instant
  def at(i: Instant): GuideStarCandidate = {
    val ldt: LocalDateTime = LocalDateTime.ofInstant(i, GuideStarCandidate.UTC)
    val epoch: Epoch       = Epoch.Julian.fromUtcDateTime(ldt).getOrElse(tracking.epoch)
    copy(tracking = tracking.at(i).fold(tracking) { c =>
      val update =
        SiderealTracking.baseCoordinates.replace(c) >>>
          SiderealTracking.epoch.replace(epoch)
      update(tracking)
    })
  }
}

object GuideStarCandidate {
  val UTC = ZoneId.of("UTC")

  val id: Lens[GuideStarCandidate, GuideStarId] =
    Focus[GuideStarCandidate](_.id)

  val tracking: Lens[GuideStarCandidate, SiderealTracking] =
    Focus[GuideStarCandidate](_.tracking)

  val brightnesses: Lens[GuideStarCandidate, SortedMap[Band, BrightnessValue]] =
    Focus[GuideStarCandidate](_.brightnesses)

  val ruwe: Lens[GuideStarCandidate, Option[BigDecimal]] =
    Focus[GuideStarCandidate](_.ruwe)

  /** A catalog star as a candidate; its id is the source id, read from the target name. */
  def fromCatalog(target: Target.Sidereal): GuideStarCandidate =
    GuideStarCandidate(
      GuideStarName
        .from(target.name.value)
        .toOption
        .flatMap(GuideStarId.fromCatalogName)
        .getOrElse(GuideStarId.GaiaDr3(-1)),
      target.tracking,
      SortedMap.from(
        SourceProfile.integratedBrightnesses
          .getOption(target.sourceProfile)
          .foldMap(_.toList)
          .collect:
            case (band, measure) if CandidateBands.contains(band) => band -> measure.value
      )
    )

  /** A catalog star as a candidate, keeping its RUWE. */
  def fromCatalogResult(result: GuideStarCatalogResult): GuideStarCandidate =
    fromCatalog(result.target).copy(ruwe = result.ruwe)

  private val MagnitudeUnits: List[Units] =
    List(
      VegaMagnitudeIsIntegratedBrightnessUnit.unit,
      ABMagnitudeIsIntegratedBrightnessUnit.unit
    )

  /**
   * One of the observation's targets as a guide star candidate.
   *
   * Only the integrated brightnesses in `CandidateBands` and in magnitudes (Vega or AB) are kept:
   * candidate brightnesses are bare magnitude values, so a flux density would be read as a
   * magnitude. AB values are taken as they are, there is no AB to Vega conversion in core (the
   * difference is a few tenths in R). A surface profile has no integrated brightnesses and yields
   * none.
   *
   * It takes a tracking rather than a target so a non-sidereal target can pass its position at the
   * observation time as a constant tracking.
   */
  def observationTarget(
    targetId:      Target.Id,
    tracking:      SiderealTracking,
    sourceProfile: SourceProfile
  ): GuideStarCandidate =
    GuideStarCandidate(
      GuideStarId.ObservationTarget(targetId.some),
      tracking,
      SortedMap.from(
        SourceProfile.integratedBrightnesses
          .getOption(sourceProfile)
          .foldMap(_.toList)
          .collect:
            case (band, measure)
                if CandidateBands.contains(band) && MagnitudeUnits.contains(measure.units) =>
              band -> measure.value
      )
    )
}

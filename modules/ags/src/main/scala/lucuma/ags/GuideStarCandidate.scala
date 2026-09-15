// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.ags

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import coulomb.*
import coulomb.syntax.*
import eu.timepit.refined.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.catalog.BandsList
import lucuma.catalog.GaiaPhotometry
import lucuma.core.enums.Band
import lucuma.core.enums.CatalogName
import lucuma.core.math.BrightnessUnits.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Epoch
import lucuma.core.math.dimensional.syntax.*
import lucuma.core.math.units.*
import lucuma.core.model.CatalogInfo
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.Target
import lucuma.core.optics.SplitEpi
import monocle.Focus
import monocle.Lens

import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneId
import scala.collection.immutable.SortedMap

/**
 * Poors' man Target.Sidereal: tracking plus the catalog's bare brightness values, no units, errors
 * or metadata. R for Altair is not stored: it is estimated from the Gaia bands on first use and
 * memoized, so a cached candidate holds catalog data only.
 */
case class GuideStarCandidate(
  id:           Long,
  tracking:     SiderealTracking,
  brightnesses: SortedMap[Band, BrightnessValue]
) derives Eq {

  def name: NonEmptyString = GuideStarName.gaiaSourceId.reverseGet(id).toNonEmptyString

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

  val id: Lens[GuideStarCandidate, Long] =
    Focus[GuideStarCandidate](_.id)

  val tracking: Lens[GuideStarCandidate, SiderealTracking] =
    Focus[GuideStarCandidate](_.tracking)

  val brightnesses: Lens[GuideStarCandidate, SortedMap[Band, BrightnessValue]] =
    Focus[GuideStarCandidate](_.brightnesses)

  // There is some loss of info converting one to the other but further
  // conversions are always the same, thus SplitEpi
  val siderealTarget: SplitEpi[Target.Sidereal, GuideStarCandidate] =
    SplitEpi(
      target =>
        GuideStarCandidate(
          GuideStarName.from(target.name.value).toOption.flatMap(_.toGaiaSourceId).getOrElse(-1),
          target.tracking,
          SortedMap.from(
            SourceProfile.integratedBrightnesses
              .getOption(target.sourceProfile)
              .foldMap(_.toList)
              .map((band, measure) => band -> measure.value)
          )
        ),
      candidate =>
        Target.Sidereal(
          candidate.name,
          candidate.tracking,
          SourceProfile.Point(
            SpectralDefinition.BandNormalized(
              None,
              candidate.brightnesses.map((band, brightness) =>
                band -> brightness.withUnit[VegaMagnitude].toMeasureTagged
              )
            )
          ),
          CatalogInfo(CatalogName.Gaia, candidate.id.toString)
        )
    )
}

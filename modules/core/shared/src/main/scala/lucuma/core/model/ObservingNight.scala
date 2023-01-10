// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats._
import lucuma.core.enums.Site
import lucuma.core.enums.TwilightType
import lucuma.core.syntax.boundedInterval._
import monocle.Focus
import monocle.Lens
import org.typelevel.cats.time._
import spire.math.Bounded

import java.time._

/**
 * An observing night is defined as the period of time from 14:00 on one day
 * until 14:00 on the following day.  In a timezone that honors daylight saving,
 * it is sometimes longer and sometimes shorter than a period of 24 hours.
 *
 * An `ObservingNight` pairs a `Site` with a `LocalObservingNight` to obtain
 * precise start/end `Instant`s.
 */
final case class ObservingNight(site: Site, toLocalObservingNight: LocalObservingNight)
    extends Night {

  /**
   * Constructs a [[TwilightBoundedNight]] for this observing night
   * according to the specified [[lucuma.core.enums.TwilightType]].
   *
   * Returns None if there's no sunset or sunrise for the specified
   * [[lucuma.core.enums.TwilightType]].
   */
  def twilightBounded(twilightType: TwilightType): Option[TwilightBoundedNight] =
    TwilightBoundedNight.fromTwilightTypeAndObservingNight(twilightType, this)

  /**
   * Constructs a [[TwilightBoundedNight]] for this observing night
   * according to the specified [[lucuma.core.enums.TwilightType]].
   *
   * Throws and exeception if there's no sunset or sunrise for the specified
   * [[lucuma.core.enums.TwilightType]].
   */
  def twilightBoundedUnsafe(twilightType: TwilightType): TwilightBoundedNight =
    twilightBounded(twilightType).get

  /** The `Interval`for the the observing night at the associated site. */
  override lazy val interval: Bounded[Instant] =
    Bounded.unsafeOpenUpper(
      toLocalObservingNight.start.atZone(site.timezone).toInstant,
      toLocalObservingNight.end.atZone(site.timezone).toInstant
    )

  /** The previous observing night. */
  def previous: ObservingNight =
    ObservingNight(site, toLocalObservingNight.previous)

  /** The next observing night. */
  def next: ObservingNight =
    ObservingNight(site, toLocalObservingNight.next)

  /** Returns the local date on which this observing night ends. */
  def toLocalDate: LocalDate =
    toLocalObservingNight.toLocalDate
}

object ObservingNight extends ObservingNightOptics {

  /**
   * Constructs the observing night that ends on the given local date for
   * for the given site.
   *
   * @group Constructors
   */
  def fromSiteAndLocalDate(s: Site, d: LocalDate): ObservingNight =
    ObservingNight(s, LocalObservingNight(d))

  /**
   * Constructs the observing night for the given site and local date time.
   *
   * @group Constructors
   */
  def fromSiteAndLocalDateTime(s: Site, d: LocalDateTime): ObservingNight =
    ObservingNight(s, LocalObservingNight.fromLocalDateTime(d))

  /**
   * Constructs the observing night that includes the given time at the
   * specified site.
   *
   * @group Constructors
   */
  def fromSiteAndInstant(s: Site, i: Instant): ObservingNight =
    ObservingNight(s, LocalObservingNight.fromSiteAndInstant(s, i))

  /** @group Typeclass Instances. */
  implicit val ShowObservingNight: Show[ObservingNight] =
    Show.fromToString

  /**
   * ObservingNight is ordered by site and local observing night.
   *
   * @group Typeclass Instances
   */
  implicit val OrderObservingNight: Order[ObservingNight] =
    Order.by(n => (n.site, n.toLocalObservingNight))

}

trait ObservingNightOptics {

  /** @group Optics */
  val site: Lens[ObservingNight, Site] =
    Focus[ObservingNight](_.site)

  /** @group Optics */
  val localObservingNight: Lens[ObservingNight, LocalObservingNight] =
    Focus[ObservingNight](_.toLocalObservingNight)

  /** @group Optics */
  val localDate: Lens[ObservingNight, LocalDate] =
    localObservingNight.andThen(LocalObservingNight.localDate)

}

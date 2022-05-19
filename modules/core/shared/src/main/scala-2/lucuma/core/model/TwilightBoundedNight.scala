// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Order
import cats.Show
import lucuma.core.`enum`.Site
import lucuma.core.`enum`.TwilightType
import lucuma.core.math.skycalc.TwilightCalc
import monocle.Getter
import spire.math.Bounded

import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime

/** The start and end of a particular night at a [[lucuma.core.`enum`.Site]]. */
sealed abstract case class TwilightBoundedNight private (
  twilightType:     TwilightType,
  toObservingNight: ObservingNight,
  interval:         Bounded[Instant]
) extends Night {

  /** Location at which the times described by this night are valid. */
  override def site: Site = toObservingNight.site

  /** The previous twilight bounded night, if it has twilight bounds. */
  def previous: Option[TwilightBoundedNight] =
    TwilightBoundedNight.fromTwilightTypeAndObservingNight(twilightType, toObservingNight.previous)

  def previousUnsafe: TwilightBoundedNight =
    previous.get

  /** The next twilight bounded night, if it has twilight bounds. */
  def next: Option[TwilightBoundedNight] =
    TwilightBoundedNight.fromTwilightTypeAndObservingNight(twilightType, toObservingNight.next)

  def nextUnsafe: TwilightBoundedNight =
    next.get

  /** Returns the local observing night for this twilight bounded night. */
  def toLocalObservingNight: LocalObservingNight =
    toObservingNight.toLocalObservingNight

  /** Returns the local date on which this twilight bounded night ends. */
  def toLocalDate: LocalDate =
    toObservingNight.toLocalDate
}

object TwilightBoundedNight extends TwilightBoundedNightOptics {

  /**
   * Constructs a [[TwilightBoundedNight]] corresponding to an [[ObservingNight]].
   *
   * Returns None if there's no sunset or sunrise for the specified
   * [[lucuma.core.`enum`.TwilightType]] and [[ObservingNight]].
   *
   * @group Constructors
   */
  def fromTwilightTypeAndObservingNight(
    twilightType:   TwilightType,
    observingNight: ObservingNight
  ): Option[TwilightBoundedNight] = {
    val site = observingNight.site
    TwilightCalc
      .forDate(twilightType, observingNight.toLocalDate.minusDays(1), site.place)
      .map(interval => new TwilightBoundedNight(twilightType, observingNight, interval) {})
  }

  /**
   * Constructs a [[TwilightBoundedNight]] corresponding to a [[ObservingNight]].
   *
   * Throws an exception if there's no sunset or sunrise for the specified
   * [[lucuma.core.`enum`.TwilightType]] and [[ObservingNight]].
   *
   * @group Constructors
   */
  def fromTwilightTypeAndObservingNightUnsafe(
    twilightType:   TwilightType,
    observingNight: ObservingNight
  ): TwilightBoundedNight =
    fromTwilightTypeAndObservingNight(twilightType, observingNight).get

  /**
   * Constructs a [[TwilightBoundedNight]] that ends on the given
   * local date for for the given site.
   *
   * Returns None if there's no sunset or sunrise for the specified
   * [[lucuma.core.`enum`.TwilightType]], [[lucuma.core.`enum`.Site]] and
   * [[java.time.LocalDate]].
   *
   * @group Constructors
   */
  def fromTwilightTypeAndSiteAndLocalDate(
    twilightType: TwilightType,
    s:            Site,
    d:            LocalDate
  ): Option[TwilightBoundedNight] =
    TwilightBoundedNight.fromTwilightTypeAndObservingNight(
      twilightType,
      ObservingNight.fromSiteAndLocalDate(s, d)
    )

  /**
   * Constructs a [[TwilightBoundedNight]] that ends on the given
   * local date for for the given site.
   *
   * Throws an exception if there's no sunset or sunrise for the specified
   * [[lucuma.core.`enum`.TwilightType]], [[lucuma.core.`enum`.Site]] and
   * [[java.time.LocalDate]].
   *
   * @group Constructors
   */
  def fromTwilightTypeAndSiteAndLocalDateUnsafe(
    twilightType: TwilightType,
    s:            Site,
    d:            LocalDate
  ): TwilightBoundedNight =
    fromTwilightTypeAndSiteAndLocalDate(twilightType, s, d).get

  /**
   * Constructs a [[TwilightBoundedNight]] that ends on the date
   * from the given local datetime for for the given site.
   *
   * Returns None if there's no sunset or sunrise for the specified
   * [[lucuma.core.`enum`.TwilightType]], [[lucuma.core.`enum`.Site]] and
   * [[java.time.LocalDateTime]].
   *
   * @group Constructors
   */
  def fromTwilightTypeAndSiteAndLocalDateTime(
    twilightType: TwilightType,
    s:            Site,
    d:            LocalDateTime
  ): Option[TwilightBoundedNight] =
    TwilightBoundedNight.fromTwilightTypeAndObservingNight(
      twilightType,
      ObservingNight.fromSiteAndLocalDateTime(s, d)
    )

  /**
   * Constructs a [[TwilightBoundedNight]] that ends on the date
   * from the given local datetime for for the given site.
   *
   * Throws an exception if there's no sunset or sunrise for the specified
   * [[lucuma.core.`enum`.TwilightType]], [[lucuma.core.`enum`.Site]] and
   * [[java.time.LocalDateTime]].
   *
   * @group Constructors
   */
  def fromTwilightTypeAndSiteAndLocalDateTimeUnsafe(
    twilightType: TwilightType,
    s:            Site,
    d:            LocalDateTime
  ): TwilightBoundedNight =
    fromTwilightTypeAndSiteAndLocalDateTime(twilightType, s, d).get

  /**
   * Constructs a [[TwilightBoundedNight]] for the observing night
   * that includes the given time at the specified site.
   *
   * Returns None if there's no sunset or sunrise for the specified
   * [[lucuma.core.`enum`.TwilightType]] and the observing night
   * that includes the given time at the specified site.
   *
   * @group Constructors
   */
  def fromTwilightTypeAndSiteAndInstant(
    twilightType: TwilightType,
    s:            Site,
    i:            Instant
  ): Option[TwilightBoundedNight] =
    TwilightBoundedNight.fromTwilightTypeAndObservingNight(
      twilightType,
      ObservingNight.fromSiteAndInstant(s, i)
    )

  /**
   * Constructs a [[TwilightBoundedNight]] for the observing night
   * that includes the given time at the specified site.
   *
   * Throws an exception if there's no sunset or sunrise for the specified
   * [[lucuma.core.`enum`.TwilightType]] and the observing night
   * that includes the given time at the specified site.
   *
   * @group Constructors
   */
  def fromTwilightTypeAndSiteAndInstantUnsafe(
    twilightType: TwilightType,
    s:            Site,
    i:            Instant
  ): TwilightBoundedNight =
    fromTwilightTypeAndSiteAndInstant(twilightType, s, i).get

  /** @group Typeclass Instances. */
  implicit val ShowTwilightBoundedNight: Show[TwilightBoundedNight] =
    Show.fromToString

  /**
   * TwilightBoundedNight is ordered by observing night and bound type.
   *
   * @group Typeclass Instances
   */
  implicit val OrderTwilightBoundedNight: Order[TwilightBoundedNight] =
    Order.by(n => (n.toObservingNight, n.twilightType))

}

trait TwilightBoundedNightOptics {

  /** @group Optics */
  val twilightType: Getter[TwilightBoundedNight, TwilightType] =
    Getter(_.twilightType)

  /** @group Optics */
  val observingNight: Getter[TwilightBoundedNight, ObservingNight] =
    Getter(_.toObservingNight)

  /** @group Optics */
  val localObservingNight: Getter[TwilightBoundedNight, LocalObservingNight] =
    observingNight.andThen(ObservingNight.localObservingNight)

  /** @group Optics */
  val site: Getter[TwilightBoundedNight, Site] =
    observingNight.andThen(ObservingNight.site)

  /** @group Optics */
  val localDate: Getter[TwilightBoundedNight, LocalDate] =
    localObservingNight.andThen(LocalObservingNight.localDate)
}

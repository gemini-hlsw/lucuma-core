// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.enum.Site
import lucuma.core.math.skycalc.Interval
import lucuma.core.math.skycalc.TwilightBoundType
import lucuma.core.math.skycalc.TwilightCalc

import cats.{ Order, Show }
import cats.effect.Sync
import cats.implicits._
import monocle.Lens
import monocle.macros.GenLens

import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime

/** The start and end of a particular night at a [[lucuma.core.enum.Site]]. */
final case class TwilightBoundedNight private (
  boundType:        TwilightBoundType,
  toObservingNight: ObservingNight,
  interval:         Interval
) extends Night {

  /** Location at which the times described by this night are valid. */
  override def site: Site = toObservingNight.site

  /** The previous twilight bounded night, if it has twilight bounds. */
  def previous: Option[TwilightBoundedNight] =
    TwilightBoundedNight.fromBoundTypeAndObservingNight(boundType, toObservingNight.previous)

  def previousUnsafe: TwilightBoundedNight =
    previous.get

  /** The next twilight bounded night, if it has twilight bounds. */
  def next: Option[TwilightBoundedNight] =
    TwilightBoundedNight.fromBoundTypeAndObservingNight(boundType, toObservingNight.next)

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

  /** Constructs a [[gem.math.TwilightBoundedNight]] corresponding to a
    * [[gem.math.ObservingNight]].
    *
    * Returns None if there's no sunset or sunrise for the specified
    * [[gsp.math.skycalc.TwilightBoundType]] and [[gem.math.ObservingNight]].
    *
    * @group Constructors
    */
  def fromBoundTypeAndObservingNight(
    boundType:      TwilightBoundType,
    observingNight: ObservingNight
  ): Option[TwilightBoundedNight] = {
    val site = observingNight.site
    TwilightCalc
      .forDate(boundType, observingNight.toLocalDate.minusDays(1), site.place)
      .map(interval => TwilightBoundedNight(boundType, observingNight, interval))
  }

  /** Constructs a [[TwilightBoundedNight]] corresponding to a [[ObservingNight]].
    *
    * Throws an exception if there's no sunset or sunrise for the specified
    * [[lucuma.core.math.skycalc.TwilightBoundType]] and [[ObservingNight]].
    *
    * @group Constructors
    */
  def fromBoundTypeAndObservingNightUnsafe(
    boundType:      TwilightBoundType,
    observingNight: ObservingNight
  ): TwilightBoundedNight =
    fromBoundTypeAndObservingNight(boundType, observingNight).get

  /** Constructs a [[TwilightBoundedNight]] that ends on the given
    * local date for for the given site.
    *
    * Returns None if there's no sunset or sunrise for the specified
    * [[lucuma.core.math.skycalc.TwilightBoundType]], [[lucuma.core.enum.Site]] and
    * [[java.time.LocalDate]].
    *
    * @group Constructors
    */
  def fromBoundTypeAndSiteAndLocalDate(
    boundType: TwilightBoundType,
    s:         Site,
    d:         LocalDate
  ): Option[TwilightBoundedNight] =
    TwilightBoundedNight.fromBoundTypeAndObservingNight(boundType,
                                                        ObservingNight.fromSiteAndLocalDate(s, d)
    )

  /** Constructs a [[TwilightBoundedNight]] that ends on the given
    * local date for for the given site.
    *
    * Throws an exception if there's no sunset or sunrise for the specified
    * [[lucuma.core.math.skycalc.TwilightBoundType]], [[lucuma.core.enum.Site]] and
    * [[java.time.LocalDate]].
    *
    * @group Constructors
    */
  def fromBoundTypeAndSiteAndLocalDateUnsafe(
    boundType: TwilightBoundType,
    s:         Site,
    d:         LocalDate
  ): TwilightBoundedNight =
    fromBoundTypeAndSiteAndLocalDate(boundType, s, d).get

  /** Constructs a [[TwilightBoundedNight]] that ends on the date
    * from the given local datetime for for the given site.
    *
    * Returns None if there's no sunset or sunrise for the specified
    * [[lucuma.core.math.skycalc.TwilightBoundType]], [[lucuma.core.enum.Site]] and
    * [[java.time.LocalDateTime]].
    *
    * @group Constructors
    */
  def fromBoundTypeAndSiteAndLocalDateTime(
    boundType: TwilightBoundType,
    s:         Site,
    d:         LocalDateTime
  ): Option[TwilightBoundedNight] =
    TwilightBoundedNight.fromBoundTypeAndObservingNight(
      boundType,
      ObservingNight.fromSiteAndLocalDateTime(s, d)
    )

  /** Constructs a [[TwilightBoundedNight]] that ends on the date
    * from the given local datetime for for the given site.
    *
    * Throws an exception if there's no sunset or sunrise for the specified
    * [[lucuma.core.math.skycalc.TwilightBoundType]], [[lucuma.core.enum.Site]] and
    * [[java.time.LocalDateTime]].
    *
    * @group Constructors
    */
  def fromBoundTypeAndSiteAndLocalDateTimeUnsafe(
    boundType: TwilightBoundType,
    s:         Site,
    d:         LocalDateTime
  ): TwilightBoundedNight =
    fromBoundTypeAndSiteAndLocalDateTime(boundType, s, d).get

  /** Constructs a [[TwilightBoundedNight]] for the observing night
    * that includes the given time at the specified site.
    *
    * Returns None if there's no sunset or sunrise for the specified
    * [[lucuma.core.math.skycalc.TwilightBoundType]] and the observing night
    * that includes the given time at the specified site.
    *
    * @group Constructors
    */
  def fromBoundTypeAndSiteAndInstant(
    boundType: TwilightBoundType,
    s:         Site,
    i:         Instant
  ): Option[TwilightBoundedNight] =
    TwilightBoundedNight.fromBoundTypeAndObservingNight(boundType,
                                                        ObservingNight.fromSiteAndInstant(s, i)
    )

  /** Constructs a [[TwilightBoundedNight]] for the observing night
    * that includes the given time at the specified site.
    *
    * Throws an exception if there's no sunset or sunrise for the specified
    * [[lucuma.core.math.skycalc.TwilightBoundType]] and the observing night
    * that includes the given time at the specified site.
    *
    * @group Constructors
    */
  def fromBoundTypeAndSiteAndInstantUnsafe(
    boundType: TwilightBoundType,
    s:         Site,
    i:         Instant
  ): TwilightBoundedNight =
    fromBoundTypeAndSiteAndInstant(boundType, s, i).get

  /** Returns a program in M that computes a [[TwilightBoundedNight]]
    * corresponding to the observing night for the instant it is executed.
    *
    * @group Constructors
    */
  def current[M[_]: Sync](boundType: TwilightBoundType, s: Site): M[Option[TwilightBoundedNight]] =
    ObservingNight.current(s).map(_.twilightBounded(boundType))

  /** @group Typeclass Instances. */
  implicit val ShowTwilightBoundedNight: Show[TwilightBoundedNight] =
    Show.fromToString

  /** TwilightBoundedNight is ordered by observing night and bound type.
    *
    * @group Typeclass Instances
    */
  implicit val OrderTwilightBoundedNight: Order[TwilightBoundedNight] =
    Order.by(n => (n.toObservingNight, n.boundType))

}

trait TwilightBoundedNightOptics {

  /** @group Optics */
  val boundType: Lens[TwilightBoundedNight, TwilightBoundType] =
    GenLens[TwilightBoundedNight](_.boundType)

  /** @group Optics */
  val observingNight: Lens[TwilightBoundedNight, ObservingNight] =
    GenLens[TwilightBoundedNight](_.toObservingNight)

  /** @group Optics */
  val localObservingNight: Lens[TwilightBoundedNight, LocalObservingNight] =
    observingNight.composeLens(ObservingNight.localObservingNight)

  /** @group Optics */
  val site: Lens[TwilightBoundedNight, Site] =
    observingNight.composeLens(ObservingNight.site)

  /** @group Optics */
  val localDate: Lens[TwilightBoundedNight, LocalDate] =
    localObservingNight.composeIso(LocalObservingNight.localDate)
}

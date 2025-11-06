// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import cats.syntax.option.*
import cats.syntax.order.*
import lucuma.core.util.NewType
import org.typelevel.cats.time.given

import java.time.Duration
import java.time.Instant

/**
  * Representation of an instant in International Atomic Time.
  * Differs from UTC in the number of leap-seconds.
  * 
  * See https://en.wikipedia.org/wiki/International_Atomic_Time
  * 
  * Wraps a java Instant adjusted for leap seconds.
  */
object TaiInstant extends NewType[Instant]:
  def fromInstant(instant: Instant): Option[TaiInstant] = 
    if Duration.between(instant, Instant.MAX).toSeconds < LeapSeconds.Max then none
    else
      TaiInstant(instant.plusSeconds(LeapSeconds.before(instant))).some

  def unsafeFromInstant(instant: Instant): TaiInstant = 
    fromInstant(instant).get

  def fromTerrestrialInstant(ttInstant: TerrestrialInstant): Option[TaiInstant] = 
    if Duration.between(Instant.MIN, ttInstant.value) < TerrestrialInstant.TaiOffset then none
    else
      TaiInstant(ttInstant.value.minus(TerrestrialInstant.TaiOffset)).some

  extension (taiInstant: TaiInstant)
    def toInstant: Instant = 
      taiInstant.value.minusSeconds(LeapSeconds.includedIn(taiInstant))
type TaiInstant = TaiInstant.Type

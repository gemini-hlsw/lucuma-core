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
  * Representation of an instant in Terrestrial Time.
  * Differs from TAI by a constant offset of 32.184 seconds.
  * 
  * See https://en.wikipedia.org/wiki/Terrestrial_Time
  * 
  * Wraps a TAI Instant adjusted by offset and leap seconds.
  */
object TerrestrialInstant extends NewType[Instant]:
  val TaiOffset: Duration = Duration.ofMillis(32184)
  
  def fromTaiInstant(taiInstant: TaiInstant): Option[TerrestrialInstant] = 
    if Duration.between(taiInstant.value, Instant.MAX) < TaiOffset then none
    else
      TerrestrialInstant(taiInstant.value.plus(TaiOffset)).some

  def fromInstant(instant: Instant): Option[TerrestrialInstant] = 
    TaiInstant.fromInstant(instant).flatMap(fromTaiInstant(_))

  def unsafeFromInstant(instant: Instant): TerrestrialInstant = 
    fromInstant(instant).get

  extension (ttInstant: TerrestrialInstant)
    def toTaiInstant: Option[TaiInstant] = 
      TaiInstant.fromTerrestrialInstant(ttInstant)

    def unsafeToTaiInstant: TaiInstant = 
      toTaiInstant.get

    def toInstant: Option[Instant] = 
      toTaiInstant.map(_.toInstant)

    def unsafeToInstant: Instant = 
      toInstant.get
type TerrestrialInstant = TerrestrialInstant.Type

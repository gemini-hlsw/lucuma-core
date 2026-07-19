// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.goa

import lucuma.core.math.Angle
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan

import java.time.Instant
import java.time.LocalDate

// Many of the fields in GOA are from OCS thus we can't do proper types for every field
// e.g. program id format is different
final case class GoaSummaryRecord(
  name:             String,
  dataLabel:        Option[String],
  ra:               Option[RightAscension],
  dec:              Option[Declination],
  instrument:       String,         // GOA archive name, e.g. "GMOS-N"; see GoaInstrument
  observationType:  GoaObservationType,
  observationClass: Option[GoaObservationClass],
  qaState:          Option[String],
  utDateTime:       Option[Instant],
  releaseDate:      Option[LocalDate],
  programId:        Option[String],
  observationId:    Option[String],
  objectName:       Option[String],
  exposure:         Option[TimeSpan],
  disperser:        Option[String],
  filter:           Option[String],
  wavelength:       Option[Wavelength],
  airmass:          Option[Double], // dimensionless ratio
  azimuth:          Option[Angle],  // signed degrees via toSignedDoubleDegrees. read via toDoubleDegrees
  elevation:        Option[Angle]   // altitude above horizon. read via toSignedDoubleDegrees
)

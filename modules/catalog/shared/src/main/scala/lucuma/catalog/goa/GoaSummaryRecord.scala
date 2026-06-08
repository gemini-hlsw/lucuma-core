// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.goa

import java.time.Instant
import java.time.LocalDate

final case class GoaSummaryRecord(
  name:             String,
  dataLabel:        Option[String],
  ra:               Option[Double],
  dec:              Option[Double],
  instrument:       String,
  observationType:  String,
  observationClass: Option[String],
  qaState:          Option[String],
  utDateTime:       Option[Instant],
  releaseDate:      Option[LocalDate],
  programId:        Option[String],
  observationId:    Option[String],
  objectName:       Option[String],
  exposure:         Option[Double],
  disperser:        Option[String],
  filter:           Option[String],
  wavelength:       Option[Double],
  airmass:          Option[Double],
  azimuth:          Option[Double],
  elevation:        Option[Double]
)

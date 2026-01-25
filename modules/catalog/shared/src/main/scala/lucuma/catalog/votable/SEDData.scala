// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.votable

import lucuma.core.enums.StellarLibrarySpectrum

case class StellarLibraryConfig(
  entries: List[(StellarLibrarySpectrum, (List[String], List[String]))]
)

case class GravityTableConfig(
  rows: List[(Double, Map[String, Double])]
)

case class SEDDataConfig(
  stellarLibrary: StellarLibraryConfig,
  gravityTable:   GravityTableConfig
)

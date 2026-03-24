// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog.simbad

import lucuma.core.enums.StellarLibrarySpectrum

/** Maps stellar library spectra to their luminosity and temperature classes. */
private[catalog] case class StellarLibraryConfig(
  entries: List[(StellarLibrarySpectrum, (List[String], List[String]))]
)

/** Maps spectral type codes to log(g) values for each luminosity class. */
private[catalog] case class GravityTableConfig(
  rows: List[(Double, Map[String, Double])]
)

private[catalog] case class SEDDataConfig(
  stellarLibrary: StellarLibraryConfig,
  gravityTable:   GravityTableConfig
)

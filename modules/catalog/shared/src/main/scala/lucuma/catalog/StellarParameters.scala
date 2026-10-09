// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.Eq
import cats.derived.*
import lucuma.core.util.Enumerated
import monocle.Focus
import monocle.Lens

/** Which catalog pipeline produced a set of stellar parameters. */
enum StellarParametersSource(val tag: String) derives Enumerated:
  /** Gaia GSP-Phot, from BP/RP spectra, G and parallax. Available for most stars. */
  case GspPhot extends StellarParametersSource("gsp_phot")

  /** Gaia ESP-HS, for O, B and A stars with Teff between 7500 K and 50000 K. */
  case EspHs extends StellarParametersSource("esp_hs")

/** Effective temperature and surface gravity of a star as reported by a catalog. */
final case class StellarParameters(
  teff:   Int,
  logG:   Double,
  source: StellarParametersSource
) derives Eq

object StellarParameters:
  val teff: Lens[StellarParameters, Int]                      = Focus[StellarParameters](_.teff)
  val logG: Lens[StellarParameters, Double]                   = Focus[StellarParameters](_.logG)
  val source: Lens[StellarParameters, StellarParametersSource] =
    Focus[StellarParameters](_.source)

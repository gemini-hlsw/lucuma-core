// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.catalog

import cats.Eq
import cats.derived.*
import coulomb.Quantity
import coulomb.integrations.cats.quantity.given
import coulomb.units.si.Kelvin
import lucuma.core.util.Enumerated
import monocle.Focus
import monocle.Lens

/** Which Gaia pipeline produced a set of stellar parameters. */
enum GaiaStellarParametersSource(val tag: String) derives Enumerated:
  /** Gaia GSP-Phot, from BP/RP spectra, G and parallax. Available for most stars. */
  case GspPhot extends GaiaStellarParametersSource("gsp_phot")

  /** Gaia ESP-HS, for O, B and A stars with Teff between 7500 K and 50000 K. */
  case EspHs extends GaiaStellarParametersSource("esp_hs")

/** Effective temperature and surface gravity of a star as reported by Gaia. */
final case class GaiaStellarParameters(
  teff:   Quantity[Int, Kelvin],
  logG:   Double,
  source: GaiaStellarParametersSource
) derives Eq

object GaiaStellarParameters:
  val teff: Lens[GaiaStellarParameters, Quantity[Int, Kelvin]]         =
    Focus[GaiaStellarParameters](_.teff)
  val logG: Lens[GaiaStellarParameters, Double]                        =
    Focus[GaiaStellarParameters](_.logG)
  val source: Lens[GaiaStellarParameters, GaiaStellarParametersSource] =
    Focus[GaiaStellarParameters](_.source)

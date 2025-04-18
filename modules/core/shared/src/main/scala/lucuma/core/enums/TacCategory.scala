// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

sealed abstract class TacGroup(val tag: String, val label: String) extends Product with Serializable

object TacGroup {
  case object SolarSystem        extends TacGroup("solar_system", "Solar System")
  case object Exoplanets         extends TacGroup("exoplanets", "Exoplanets")
  case object GalacticLocalGroup extends TacGroup("galactic_local_group", "Galactic/Local Group")
  case object Extragalactic      extends TacGroup("extragalactic", "Extragalactic")

  implicit val TacGroupEnumerated: Enumerated[TacGroup] =
    Enumerated.from(SolarSystem, Exoplanets, GalacticLocalGroup, Extragalactic).withTag(_.tag)
}

sealed abstract class TacCategory(val tag: String, val label: String, val group: TacGroup)
    extends Product
    with Serializable

object TacCategory {
  import TacGroup._

  case object SmallBodies
      extends TacCategory("small_bodies", "Small Bodies: Asteroids, Comets, Moons, Kuiper Belt", SolarSystem)
  case object PlanetaryAtmospheres extends TacCategory("planetary_atmospheres", "Planetary Atmospheres", SolarSystem)
  case object PlanetarySurfaces    extends TacCategory("planetary_surfaces", "Planetary Surfaces", SolarSystem)
  case object SolarSystemOther     extends TacCategory("solar_system_other", "Solar System Other", SolarSystem)

  case object ExoplanetRadialVelocities
      extends TacCategory("exoplanet_radial_velocities", "Exoplanet Radial Velocities", Exoplanets)
  case object ExoplanetAtmospheresActivity
      extends TacCategory("exoplanet_atmospheres_activity", "Exoplanet Atmospheres/Activity", Exoplanets)
  case object ExoplanetTransits
      extends TacCategory("exoplanet_transits", "Exoplanet Transits, Rossiter McLaughlin", Exoplanets)
  case object ExoplanetHostStar
      extends TacCategory("exoplanet_host_star", "Exoplanet Host Star Properties/Connections", Exoplanets)
  case object ExoplanetOther extends TacCategory("exoplanet_other", "Exoplanet Other", Exoplanets)

  case object StellarAstrophysics
      extends TacCategory("stellar_astrophysics", "Stellar Astrophysics, Evolution, Supernovae, Abundances",
                          GalacticLocalGroup
      )
  case object StellarPopulations
      extends TacCategory("stellar_populations", "Stellar Populations, Clusters, Chemical Evolution", GalacticLocalGroup)
  case object StarFormation extends TacCategory("star_formation", "Star Formation", GalacticLocalGroup)
  case object GaseousAstrophysics
      extends TacCategory("gaseous_astrophysics", "Gaseous Astrophysics, H II regions, PN, ISM, SN remnants, Novae",
                          GalacticLocalGroup
      )
  case object StellarRemnants
      extends TacCategory("stellar_remnants", "Stellar Remnants/Compact Objects, WD, NS, BH", GalacticLocalGroup)
  case object GalacticOther extends TacCategory("galactic_other", "Galactic Other", GalacticLocalGroup)

  case object Cosmology
      extends TacCategory("cosmology", "Cosmology, Fundamental Physics, Large Scale Structure", Extragalactic)
  case object ClustersOfGalaxies extends TacCategory("clusters_of_galaxies", "Clusters/Groups of Galaxies", Extragalactic)
  case object HighZUniverse      extends TacCategory("high_z_universe", "High-z Universe", Extragalactic)
  case object LowZUniverse       extends TacCategory("low_z_universe", "Low-z Universe", Extragalactic)
  case object ActiveGalaxies     extends TacCategory("active_galaxies", "Active Galaxies, Quasars, SMBH", Extragalactic)
  case object ExtragalacticOther extends TacCategory("extragalactic_other", "Extragalactic Other", Extragalactic)

  implicit val TacCategoryEnumerated: Enumerated[TacCategory] =
    Enumerated.from(
      SmallBodies,
      PlanetaryAtmospheres,
      PlanetarySurfaces,
      SolarSystemOther,
      ExoplanetRadialVelocities,
      ExoplanetAtmospheresActivity,
      ExoplanetTransits,
      ExoplanetHostStar,
      ExoplanetOther,
      StellarAstrophysics,
      StellarPopulations,
      StarFormation,
      GaseousAstrophysics,
      StellarRemnants,
      GalacticOther,
      Cosmology,
      ClustersOfGalaxies,
      HighZUniverse,
      LowZUniverse,
      ActiveGalaxies,
      ExtragalacticOther
    ).withTag(_.tag)
}

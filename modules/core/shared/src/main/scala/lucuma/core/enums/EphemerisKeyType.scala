// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for Non-sidereal target lookup type.
 * @group Enumerations
 */
enum EphemerisKeyType(val tag: String, val shortName: String, val longName: String) derives Enumerated:
  /** @group Constructors */ case Comet extends EphemerisKeyType("Comet", "Comet", "Horizons Comet")
  /** @group Constructors */ case AsteroidNew extends EphemerisKeyType("AsteroidNew", "Asteroid New", "Horizons Asteroid (New Format)")
  /** @group Constructors */ case AsteroidOld extends EphemerisKeyType("AsteroidOld", "Asteroid Old", "Horizons Asteroid (Old Format)")
  /** @group Constructors */ case MajorBody extends EphemerisKeyType("MajorBody", "Major Body", "Horizons Major Body")
  /** @group Constructors */ case UserSupplied extends EphemerisKeyType("UserSupplied", "User Supplied", "User Supplied")

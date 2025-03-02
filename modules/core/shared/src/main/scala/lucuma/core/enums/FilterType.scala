// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for filter types.
 * @group Enumerations
 */
enum FilterType(val tag: String) derives Enumerated:
  /** @group Constructors */ case BroadBand extends FilterType("BroadBand")
  /** @group Constructors */ case NarrowBand extends FilterType("NarrowBand")
  /** @group Constructors */ case Combination extends FilterType("Combination")
  /** @group Constructors */ case Spectroscopic extends FilterType("Spectroscopic")
  /** @group Constructors */ case Engineering extends FilterType("Engineering")

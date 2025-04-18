// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for MOS pre-imaging category.
 * @group Enumerations (Generated)
 */
enum MosPreImaging(val tag: String, val description: String, val toBoolean: Boolean) derives Enumerated:
  /** @group Constructors */ case IsMosPreImaging extends MosPreImaging("IsMosPreImaging", "Is MOS Pre-imaging", true)
  /** @group Constructors */ case IsNotMosPreImaging extends MosPreImaging("IsNotMosPreImaging", "Is Not MOS Pre-Imaging", false)

// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Display
import lucuma.core.util.Enumerated

/**
 * Enumerated type for dataset QA state.
 * @group Enumerations
 */
enum DatasetQaState(val tag: String, val name: String) derives Enumerated, Display:
  def shortName: String = name
  def longName: String = name

  /** @group Constructors */ case Pass   extends DatasetQaState("pass",   "Pass")
  /** @group Constructors */ case Usable extends DatasetQaState("usable", "Usable")
  /** @group Constructors */ case Fail   extends DatasetQaState("fail",   "Fail")

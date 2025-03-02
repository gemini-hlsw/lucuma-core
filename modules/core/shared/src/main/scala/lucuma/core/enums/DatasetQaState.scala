// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

/**
 * Enumerated type for dataset QA state.
 * @group Enumerations
 */
enum DatasetQaState(val tag: String) derives Enumerated:
  def shortName: String = tag
  def longName: String = tag

  /** @group Constructors */ case Pass   extends DatasetQaState("Pass")
  /** @group Constructors */ case Usable extends DatasetQaState("Usable")
  /** @group Constructors */ case Fail   extends DatasetQaState("Fail")

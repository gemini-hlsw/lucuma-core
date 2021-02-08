// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import eu.timepit.refined.auto._

object Asterism extends WithId {
  protected val idTag = 'a'
}

object Configuration extends WithId {
  protected val idTag = 'x'
}

object Observation extends WithId {
  protected val idTag = 'o'
}

object Program extends WithId {
  protected val idTag = 'p'
}

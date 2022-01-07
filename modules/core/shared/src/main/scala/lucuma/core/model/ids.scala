// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import eu.timepit.refined.auto._

object Atom              extends WithId('m')
object Configuration     extends WithId('c')
object Dataset           extends WithId('d')
object ExecutionEvent    extends WithId('e')
object Observation       extends WithId('o')
object Program           extends WithId('p')
object Step              extends WithId('s')
object TargetEnvironment extends WithId('v')

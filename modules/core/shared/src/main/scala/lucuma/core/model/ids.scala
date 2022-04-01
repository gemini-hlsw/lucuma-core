// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import eu.timepit.refined.auto._
import lucuma.core.util.WithGid
import lucuma.core.util.WithUid

object Atom           extends WithUid('a')
object Configuration  extends WithGid('c')
object Dataset        extends WithGid('d')
object ExecutionEvent extends WithGid('e')
object Observation    extends WithGid('o')
object Program        extends WithGid('p')
object Step           extends WithUid('s')
object Visit          extends WithUid('v')

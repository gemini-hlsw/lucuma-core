// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import eu.timepit.refined.auto._
import lucuma.core.util.WithGid
import lucuma.core.util.WithUid
import lucuma.refined._

object Attachment     extends WithGid('a'.refined)
object Configuration  extends WithGid('c'.refined)
object Dataset        extends WithGid('d'.refined)
object ExecutionEvent extends WithGid('e'.refined)
object Observation    extends WithGid('o'.refined)
object Program        extends WithGid('p'.refined)
object Visit          extends WithUid('v'.refined)

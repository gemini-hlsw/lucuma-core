// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.util.WithGid
import lucuma.refined.*

object Configuration  extends WithGid('c'.refined)
object Group          extends WithGid('g'.refined)
object ObsAttachment  extends WithGid('a'.refined)
object Observation    extends WithGid('o'.refined)
object Program        extends WithGid('p'.refined)
object Visit          extends WithGid('v'.refined)

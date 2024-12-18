// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.util.WithGid
import lucuma.refined.*

object Attachment       extends WithGid('a'.refined)
object CallForProposals extends WithGid('c'.refined)
object Group            extends WithGid('g'.refined)
object Observation      extends WithGid('o'.refined)
object Program          extends WithGid('p'.refined)
object ProgramUser      extends WithGid('m'.refined)
object Visit            extends WithGid('v'.refined)

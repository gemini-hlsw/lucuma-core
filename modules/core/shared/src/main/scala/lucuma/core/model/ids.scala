// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.refined.auto.*
import lucuma.core.util.WithGid

object Attachment       extends WithGid('a'.refined)
object CallForProposals extends WithGid('c'.refined)
object Group            extends WithGid('g'.refined)
object Observation      extends WithGid('o'.refined)
object Program          extends WithGid('p'.refined)
object ProgramNote      extends WithGid('n'.refined)
object ProgramUser      extends WithGid('m'.refined)
object Visit            extends WithGid('v'.refined)

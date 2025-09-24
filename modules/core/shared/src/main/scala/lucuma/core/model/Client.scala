// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import lucuma.core.refined.auto.*
import lucuma.core.util.WithUid

// This should be deprecated but actually doing so causes a hardship in the ODB leaf mapping, which the compiler insists
// on inlining and reporting the deprecation warning despite the nowarn annotation.

//@deprecated(message = "Use lucuma.core.util.IdempotencyKey instead.")
object Client extends WithUid('c'.refined)
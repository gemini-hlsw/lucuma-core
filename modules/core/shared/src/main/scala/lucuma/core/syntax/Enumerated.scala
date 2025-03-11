// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.syntax

import lucuma.core.util.Enumerated

trait ToEnumeratedOps:
  extension [A](a: A)(using ev: Enumerated[A])
    def tag: String = ev.tag(a)

object enumerated extends ToEnumeratedOps

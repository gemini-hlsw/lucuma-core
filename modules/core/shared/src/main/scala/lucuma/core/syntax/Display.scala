// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.syntax

import lucuma.core.util.Display

trait ToDisplayOps:
  extension [A](value: A)(using d: Display[A])
    def shortName: String = d.shortName(value)
    def longName : String = d.longName(value)

object display extends ToDisplayOps

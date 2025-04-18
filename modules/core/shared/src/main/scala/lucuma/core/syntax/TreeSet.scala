// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.syntax

import scala.collection.immutable.TreeSet

trait ToTreeSetCompanionOps {
  extension(c: TreeSet.type)
    /** Creates a `TreeSet` from a `List`, provided an `Ordering` is available. */
    def fromList[A: Ordering](lst: List[A]): TreeSet[A] =
      TreeSet(lst*)
}

object treesetcompanion extends ToTreeSetCompanionOps

// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.syntax

import lucuma.core.util.Enumerated

final class EnumeratedOps[A: Enumerated](a: A) {

  def tag: String =
    Enumerated[A].tag(a)

}

trait ToEnumeratedOps {

  implicit def toEnumeratedOps[A: Enumerated](a: A): EnumeratedOps[A] =
    new EnumeratedOps[A](a)

}

object enumerated extends ToEnumeratedOps

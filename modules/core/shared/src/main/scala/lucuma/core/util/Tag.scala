// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.util

import shapeless.tag
import shapeless.tag.@@

/**
 * Type class placing a tag `U` on type `T`. Helps produce tagged instances of `T`.
 */
trait Tag[T, U] {
  def apply(t: T): T @@ U = tag[U](t)
}

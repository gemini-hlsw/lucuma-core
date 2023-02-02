// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core

import lucuma.core.math.syntax.ToIntOps
import lucuma.core.optics.syntax.ToPrismOps

/**
 * Syntax classes for extension methods, organized à la cats. Each syntax class has an associated
 * conversion trait and module that extends it; and the `all` module which extends all conversions
 * traits.
 */
package object syntax {
  object all
      extends NonEmptyOps
      with ToDisplayOps
      with TimeOps
      with ToEnumeratedOps
      with ToIntOps
      with ToPrismOps
      with ToStringOps
      with ToTreeMapCompanionOps
      with ToTreeMapOps
      with ToTreeSetCompanionOps
}

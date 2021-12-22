// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.math

import coulomb.unitops.UnitString

package object dimensional {

  implicit class UnitStringModuleOps(val unitStringModule: UnitString.type) extends AnyVal {

    /**
     * Convenience method for creating UnitString instances.
     */
    def apply[U](fullName: String, abbreviation: String): UnitString[U] =
      new UnitString[U] {
        override val full: String = fullName
        override val abbv: String = abbreviation
      }

    /**
     * Convenience method for creating UnitString instances.
     */
    def apply[U](abbreviation: String): UnitString[U] =
      apply(abbreviation, abbreviation)
  }
}

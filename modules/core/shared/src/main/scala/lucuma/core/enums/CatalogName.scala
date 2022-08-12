// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

sealed abstract class CatalogName(val tag: String) extends Product with Serializable

object CatalogName {
  case object Simbad extends CatalogName("simbad")
  case object Gaia extends CatalogName("gaia")

  /** @group Typeclass Instances */
  implicit val CatalogNameEnumerated: Enumerated[CatalogName] =
    Enumerated.from(Simbad, Gaia).withTag(_.tag)

}

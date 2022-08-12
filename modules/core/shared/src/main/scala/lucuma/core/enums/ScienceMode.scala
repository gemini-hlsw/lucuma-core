// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

sealed abstract class ScienceMode(val tag: String) extends Product with Serializable

object ScienceMode {
  case object Imaging      extends ScienceMode("imaging")
  case object Spectroscopy extends ScienceMode("spectroscopy")

  implicit val ScienceModeEnumerated: Enumerated[ScienceMode] =
    Enumerated.from(Imaging, Spectroscopy).withTag(_.tag)
}


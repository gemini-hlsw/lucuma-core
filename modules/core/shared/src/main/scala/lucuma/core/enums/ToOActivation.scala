// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

sealed abstract class ToOActivation(val tag: String, val label: String) extends Product with Serializable

object ToOActivation {
  case object None     extends ToOActivation("none", "None")
  case object Standard extends ToOActivation("standard", "Standard")
  case object Rapid    extends ToOActivation("rapid", "Rapid")

  implicit val ToOActivationEnumerated: Enumerated[ToOActivation] =
    Enumerated.from(None, Standard, Rapid).withTag(_.tag)
}

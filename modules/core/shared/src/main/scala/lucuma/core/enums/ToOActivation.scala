// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

sealed abstract class ToOActivation(val label: String) extends Product with Serializable

object ToOActivation {
  case object None     extends ToOActivation("None")
  case object Standard extends ToOActivation("Standard")
  case object Rapid    extends ToOActivation("Rapid")

  implicit val ToOActivationEnumerated: Enumerated[ToOActivation] =
    Enumerated.of(None, Standard, Rapid)
}

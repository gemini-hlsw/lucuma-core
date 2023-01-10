// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

sealed abstract class SequenceType(val tag: String) extends Product with Serializable

object SequenceType {
  case object Acquisition extends SequenceType("acquisition")
  case object Science     extends SequenceType("science")

  implicit val SequenceTypeEnumerated: Enumerated[SequenceType] =
    Enumerated.from(Acquisition, Science).withTag(_.tag)
}

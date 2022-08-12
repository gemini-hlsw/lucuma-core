// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

sealed abstract class FocalPlane(val tag: String) extends Product with Serializable

object FocalPlane {
  case object SingleSlit   extends FocalPlane("single_slit")
  case object MultipleSlit extends FocalPlane("multiple_slit")
  case object IFU          extends FocalPlane("ifu")

  /** @group Typeclass Instances */
  implicit val FocalPlaneEnumerated: Enumerated[FocalPlane] =
    Enumerated.from(SingleSlit, MultipleSlit, IFU).withTag(_.tag)
}

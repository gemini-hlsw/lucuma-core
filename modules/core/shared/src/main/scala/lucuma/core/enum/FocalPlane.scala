// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import lucuma.core.util.Enumerated

sealed trait FocalPlane extends Product with Serializable

object FocalPlane {
  case object SingleSlit   extends FocalPlane
  case object MultipleSlit extends FocalPlane
  case object IFU          extends FocalPlane

  /** @group Typeclass Instances */
  implicit val FocalPlaneEnumerated: Enumerated[FocalPlane] =
    Enumerated.of(SingleSlit, MultipleSlit, IFU)
}

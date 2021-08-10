// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enum

import lucuma.core.util.Enumerated

sealed trait SpatialProfileType extends Product with Serializable

object SpatialProfileType {
  final case object PointSource    extends SpatialProfileType
  final case object UniformSource  extends SpatialProfileType
  final case object GaussianSource extends SpatialProfileType

  /** @group Typeclass Instances */
  implicit val SpatialProfileTypeEnumerated: Enumerated[SpatialProfileType] =
    Enumerated.of(PointSource, UniformSource, GaussianSource)

}

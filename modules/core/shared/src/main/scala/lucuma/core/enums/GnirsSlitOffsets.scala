// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.enums

import cats.data.NonEmptyList
import lucuma.core.math.Offset

enum GnirsSlitOffsets(val offsetsType: GnirsSlitOffsetsType, val offsets: NonEmptyList[Offset]):
  case OnSlit(value: NonEmptyList[Offset.Q]) extends GnirsSlitOffsets(GnirsSlitOffsetsType.OnSlit, value.map(q => Offset(Offset.P.Zero, q)))
  case OffSlit(value: NonEmptyList[Offset]) extends GnirsSlitOffsets(GnirsSlitOffsetsType.OffSlit, value)

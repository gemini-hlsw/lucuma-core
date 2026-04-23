// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gnirs

import cats.Eq
import cats.derived.*
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsDecker
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuOther
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsReadMode
import lucuma.core.util.TimeSpan

final case class GnirsDynamicConfig(
  exposure:          TimeSpan,
  filter:            GnirsFilter,
  decker:            GnirsDecker,
  fpu:               Either[GnirsFpuSlit, GnirsFpuOther],
  acquisitionMirror: GnirsAcquisitionMirrorMode,
  camera:            GnirsCamera,
  focus:             GnirsFocus,
  readMode:          GnirsReadMode
) derives Eq

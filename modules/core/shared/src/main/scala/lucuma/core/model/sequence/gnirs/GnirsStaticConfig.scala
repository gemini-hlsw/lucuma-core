// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gnirs

import cats.Eq
import cats.derived.*
import lucuma.core.enums.GnirsWellDepth
import monocle.Focus
import monocle.Lens

final case class GnirsStaticConfig(
  wellDepth: GnirsWellDepth
) derives Eq

object GnirsStaticConfig:
  val wellDepth: Lens[GnirsStaticConfig, GnirsWellDepth] =
    Focus[GnirsStaticConfig](_.wellDepth)
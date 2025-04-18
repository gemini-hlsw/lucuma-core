// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.derived.*
import lucuma.core.enums.MountGuideOption
import monocle.Focus
import monocle.Lens

/** Data type for guide config. */
case class TelescopeGuideConfig(
  mountGuide:  MountGuideOption,
  m1Guide:     M1GuideConfig,
  m2Guide:     M2GuideConfig,
  dayTimeMode: Option[Boolean], // This should not be an Option but we make it optional for backward compatibility with TCC
  probeGuide:  Option[ProbeGuide]
) derives Eq

object TelescopeGuideConfig {
  val mountGuide: Lens[TelescopeGuideConfig, MountGuideOption] =
    Focus[TelescopeGuideConfig](_.mountGuide)

  val m1Guide: Lens[TelescopeGuideConfig, M1GuideConfig] =
    Focus[TelescopeGuideConfig](_.m1Guide)

  val m2Guide: Lens[TelescopeGuideConfig, M2GuideConfig] =
    Focus[TelescopeGuideConfig](_.m2Guide)

  val dayTimeMode: Lens[TelescopeGuideConfig, Option[Boolean]] =
    Focus[TelescopeGuideConfig](_.dayTimeMode)

  val probeGuide: Lens[TelescopeGuideConfig, Option[ProbeGuide]] =
    Focus[TelescopeGuideConfig](_.probeGuide)
}

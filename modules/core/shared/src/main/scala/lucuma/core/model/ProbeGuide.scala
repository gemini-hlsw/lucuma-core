// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model

import cats.Eq
import cats.Show
import cats.derived.*
import lucuma.core.enums.GuideProbe

/** Data type for guide config. */
case class ProbeGuide(from: GuideProbe, to: GuideProbe) derives Eq, Show

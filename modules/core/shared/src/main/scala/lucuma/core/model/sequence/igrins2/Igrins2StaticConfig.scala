// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.igrins2

import cats.Eq

case class Igrins2StaticConfig()

object Igrins2StaticConfig:
  given Eq[Igrins2StaticConfig] =
    Eq.allEqual

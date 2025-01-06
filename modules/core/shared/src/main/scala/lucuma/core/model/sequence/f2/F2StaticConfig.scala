// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.f2

import cats.Eq
import cats.syntax.all.*
import lucuma.core.enums.*
import monocle.Focus
import monocle.Lens

case class F2StaticConfig(
  mosPreImaging:          Boolean,
  useElectronicOffseting: Boolean,
  customSlitWidth:        Option[F2CustomSlitWidth]
)

object F2StaticConfig:
  given Eq[F2StaticConfig] =
    Eq.by(x => (x.mosPreImaging, x.useElectronicOffseting, x.customSlitWidth))

  /** @group Optics */
  val mosPreImaging: Lens[F2StaticConfig, Boolean] =
    Focus[F2StaticConfig](_.mosPreImaging)

  /** @group Optics */
  val useElectronicOffsetting: Lens[F2StaticConfig, Boolean] =
    Focus[F2StaticConfig](_.useElectronicOffseting)

  /** @group Optics */
  val customSlitWidth: Lens[F2StaticConfig, Option[F2CustomSlitWidth]] =
    Focus[F2StaticConfig](_.customSlitWidth)


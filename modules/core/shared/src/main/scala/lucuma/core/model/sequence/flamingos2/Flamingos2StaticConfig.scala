// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.flamingos2

import cats.Eq
import lucuma.core.enums.MosPreImaging
import monocle.Focus
import monocle.Lens

case class Flamingos2StaticConfig(
  mosPreImaging:          MosPreImaging,
  useElectronicOffsetting: Boolean
)

object Flamingos2StaticConfig:
  given Eq[Flamingos2StaticConfig] =
    Eq.by(x => (x.mosPreImaging, x.useElectronicOffsetting))

  /** @group Optics */
  val mosPreImaging: Lens[Flamingos2StaticConfig, MosPreImaging] =
    Focus[Flamingos2StaticConfig](_.mosPreImaging)

  /** @group Optics */
  val useElectronicOffsetting: Lens[Flamingos2StaticConfig, Boolean] =
    Focus[Flamingos2StaticConfig](_.useElectronicOffsetting)

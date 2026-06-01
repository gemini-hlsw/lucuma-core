// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gnirs

import cats.Eq
import cats.derived.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.math.Offset
import lucuma.core.math.syntax.bigDecimal.*
import lucuma.core.util.TimeSpan
import monocle.Focus
import monocle.Iso
import monocle.Prism
import monocle.macros.GenPrism

enum GnirsAcquisitionSkyOffset derives Eq:
  case Disabled
  case Enabled(val offset: Offset)

object GnirsAcquisitionSkyOffset:
  object Enabled:
    val Default: GnirsAcquisitionSkyOffset.Enabled =
      GnirsAcquisitionSkyOffset.Enabled(Offset(0.pArcsec, -2.qArcsec))

    val offset: Iso[GnirsAcquisitionSkyOffset.Enabled, Offset] = Focus[GnirsAcquisitionSkyOffset.Enabled](_.offset)

  def forAcquisitionIntegrationTime(exposureTime: TimeSpan, coadds: PosInt): GnirsAcquisitionSkyOffset =
    val integrationTimeSeconds: BigDecimal = exposureTime.toSeconds * coadds.value
    if integrationTimeSeconds >= 10 then Enabled.Default
    else Disabled

  val enabled: Prism[GnirsAcquisitionSkyOffset, Enabled] = GenPrism[GnirsAcquisitionSkyOffset, Enabled]

  val offset: Prism[GnirsAcquisitionSkyOffset, Offset] =
    enabled.andThen(Enabled.offset)


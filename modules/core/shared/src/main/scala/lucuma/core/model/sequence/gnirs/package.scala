// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.core.model.sequence.gnirs

import cats.data.NonEmptyList
import cats.syntax.order.*
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.SlitOffsetMode
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.math.syntax.bigDecimal.*
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.TelescopeConfigAlongSlit

// ATTENTION: This logic is duplicated in the DB view in the ODB. Modify it there too if it's changed here.
// The actual requirements actually use the wavelength instead of the filter, but the default wavelength
// is the filter's optimalWavelength, so we shortcircuit and use the filter directly.
def alongSlitDefaultTelescopeConfigs(prism: GnirsPrism, camera: GnirsCamera, wavelength: GnirsGratingWavelength): NonEmptyList[TelescopeConfigAlongSlit] =
  val LongCameraCutoff: Wavelength = Wavelength.fromIntNanometers(2500).get
  val arcSecs: NonEmptyList[BigDecimal] =
    if prism === GnirsPrism.Sxd || prism === GnirsPrism.Lxd then
      NonEmptyList.of(-1, 2, 2, -1)
    else if camera === GnirsCamera.ShortBlue || camera === GnirsCamera.ShortRed then
      NonEmptyList.of(2, -4, -4, 2)
    else if wavelength.value >= LongCameraCutoff then
      NonEmptyList.of(-3, 3, 3, -3)
    else
      NonEmptyList.of(-1, 5, 5, -1)
  arcSecs.map(arcSec => TelescopeConfigAlongSlit(arcSec.qArcsec, StepGuideState.Enabled))

val OnSkyDefaultTelescopeConfigs: NonEmptyList[TelescopeConfig] =
  NonEmptyList.of(
    TelescopeConfig(Offset.Zero, StepGuideState.Enabled),
    TelescopeConfig(Offset(30.pArcsec, 0.qArcsec), StepGuideState.Disabled),
    TelescopeConfig(Offset(30.pArcsec, 0.qArcsec), StepGuideState.Disabled),
    TelescopeConfig(Offset.Zero, StepGuideState.Enabled)
  )

def defaultSlitTelescopeConfigs(mode: SlitOffsetMode, prism: GnirsPrism, camera: GnirsCamera, wavelength: GnirsGratingWavelength): SlitTelescopeConfigs =
  mode match
    case SlitOffsetMode.NodAlongSlit => SlitTelescopeConfigs.AlongSlit(alongSlitDefaultTelescopeConfigs(prism, camera, wavelength))
    case SlitOffsetMode.NodToSky     => SlitTelescopeConfigs.ToSky(OnSkyDefaultTelescopeConfigs)